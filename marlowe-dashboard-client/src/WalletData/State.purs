module WalletData.State
  ( mkInitialState
  , defaultWalletDetails
  , handleAction
  , adaToken
  , getAda
  , parseWalletNickname
  , parseWalletId
  , parsePlutusAppId
  , parseWalletDetails
  ) where

import Prelude
import Capability.MainFrameLoop (callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe, lookupWalletDetails, lookupWalletInfo)
import Capability.MarloweStorage (class ManageMarloweStorage, insertIntoWalletLibrary)
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Control.Monad.Reader (class MonadAsk)
import Dashboard.Types (Action(..)) as Dashboard
import Data.Array (any)
import Data.BigInteger (BigInteger)
import Data.Char.Unicode (isAlphaNum)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use)
import Data.Map (isEmpty, filter, insert, lookup, member)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (toCharArray)
import Data.UUID (emptyUUID, parseUUID)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, query, tell)
import Halogen.Query.HalogenM (mapAction)
import Input.Text as TInput
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg, dashboardWalletDataIdSlot, dashboardWalletDataNicknameSlot)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Semantics (Assets, Token(..))
import Network.RemoteData (RemoteData(..), fromEither)
import Toast.Types (errorToast, successToast)
import Types (WebData)
import WalletData.Lenses (_cardSection, _remoteWalletInfo, _walletId, _walletLibrary, _walletNickname)
import WalletData.Types (Action(..), CardSection(..), PubKeyHash(..), State, Wallet(..), WalletIdError(..), WalletInfo(..), WalletLibrary, WalletNickname, WalletNicknameError(..), WalletDetails)

mkInitialState :: WalletLibrary -> State
mkInitialState walletLibrary =
  let
    remoteWalletInfo = NotAsked
  in
    { walletLibrary
    , cardSection: Home
    , walletNickname: parseWalletNickname walletLibrary ""
    , walletId: parseWalletId remoteWalletInfo walletLibrary ""
    , remoteWalletInfo
    }

defaultWalletDetails :: WalletDetails
defaultWalletDetails =
  { walletNickname: mempty
  , companionAppId: PlutusAppId emptyUUID
  , marloweAppId: PlutusAppId emptyUUID
  , walletInfo: defaultWalletInfo
  , assets: mempty
  , previousCompanionAppState: Nothing
  }

defaultWalletInfo :: WalletInfo
defaultWalletInfo =
  WalletInfo
    { wallet: Wallet zero
    , pubKey: ""
    , pubKeyHash: PubKeyHash ""
    }

handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  ManageMarlowe m =>
  ManageMarloweStorage m =>
  Toast m =>
  MonadClipboard m =>
  Action -> HalogenM State Action ChildSlots Msg m Unit
handleAction CloseWalletDataCard = callMainFrameAction $ MainFrame.DashboardAction $ Dashboard.CloseCard

handleAction (SetCardSection cardSection) = do
  case cardSection of
    NewWallet _ -> do
      assign _remoteWalletInfo NotAsked
      void
        $ query dashboardWalletDataIdSlot unit
        $ tell TInput.Reset
      void
        $ query dashboardWalletDataNicknameSlot unit
        $ tell TInput.Reset
    _ -> pure unit
  assign _cardSection cardSection

handleAction (SaveWallet mTokenName walletDetails@{ walletNickname }) = do
  modifying _walletLibrary (insert walletNickname walletDetails)
  insertIntoWalletLibrary walletDetails
  -- if a tokenName was also passed, we need to update the contract setup data
  for_ mTokenName \tokenName -> callMainFrameAction $ MainFrame.DashboardAction $ Dashboard.SetContactForRole tokenName walletNickname

handleAction CancelNewContactForRole = pure unit -- handled in Dashboard.State

handleAction (WalletNicknameChanged value) = assign _walletNickname value

handleAction (WalletIdChanged value) = do
  -- note we assign the value _first_ so that the InputField value is set - otherwise the
  -- validation feedback is wrong while the rest is happening
  assign _walletId value
  handleAction $ SetRemoteWalletInfo NotAsked
  -- if this is a valid contract ID ...
  for_ value \walletId -> do
    handleAction $ SetRemoteWalletInfo Loading
    -- .. lookup wallet info
    ajaxWalletInfo <- lookupWalletInfo walletId
    handleAction $ SetRemoteWalletInfo $ fromEither ajaxWalletInfo

handleAction (SetRemoteWalletInfo remoteWalletInfo) = assign _remoteWalletInfo remoteWalletInfo

handleAction (UseWallet walletNickname companionAppId) = do
  ajaxWalletDetails <- lookupWalletDetails companionAppId
  case ajaxWalletDetails of
    Right walletDetails -> do
      let
        walletDetailsWithNickname = set _walletNickname walletNickname walletDetails
      walletLibrary <- use _walletLibrary
      callMainFrameAction $ MainFrame.EnterDashboardState walletLibrary walletDetailsWithNickname
    _ -> do
      addToast $ errorToast "Unable to use this wallet." $ Just "Details for this wallet could not be loaded."

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"

------------------------------------------------------------
adaToken :: Token
adaToken = Token "" ""

getAda :: Assets -> BigInteger
getAda assets = fromMaybe zero $ lookup "" =<< lookup "" (unwrap assets)

parseWalletNickname :: WalletLibrary -> WalletNickname -> Either WalletNicknameError String
parseWalletNickname _ "" = Left EmptyWalletNickname

parseWalletNickname walletLibrary walletNickname =
  if member walletNickname walletLibrary then
    Left DuplicateWalletNickname
  else
    if any (\char -> not $ isAlphaNum char) $ toCharArray walletNickname then
      Left BadWalletNickname
    else
      Right walletNickname

parseWalletId :: WebData WalletInfo -> WalletLibrary -> String -> Either WalletIdError PlutusAppId
parseWalletId _ _ "" = Left EmptyWalletId

parseWalletId remoteDataWalletInfo walletLibrary walletIdString = case parsePlutusAppId walletIdString of
  Nothing -> Left InvalidWalletId
  Just plutusAppId
    | not $ isEmpty $ filter (\walletDetails -> walletDetails.companionAppId == plutusAppId) walletLibrary -> Left DuplicateWalletId
    | otherwise -> case remoteDataWalletInfo of
      Success _ -> Right plutusAppId
      Failure _ -> Left NonexistentWalletId
      _ -> Left UnconfirmedWalletId

parsePlutusAppId :: String -> Maybe PlutusAppId
parsePlutusAppId plutusAppIdString = case parseUUID plutusAppIdString of
  Just uuid -> Just $ PlutusAppId uuid
  Nothing -> Nothing

parseWalletDetails :: State -> Maybe WalletDetails
parseWalletDetails { remoteWalletInfo, walletId, walletNickname } = case remoteWalletInfo, walletId, walletNickname of
  Success walletInfo, Right companionAppId, Right nickname ->
    Just
      { walletNickname: nickname
      , companionAppId
      , marloweAppId: PlutusAppId emptyUUID
      , walletInfo
      , assets: mempty
      -- this property shouldn't be necessary, but at the moment we are getting too many update notifications
      -- through the PAB - so until that bug is fixed, we use this to check whether an update notification
      -- really has changed anything
      , previousCompanionAppState: Nothing
      }
  _, _, _ -> Nothing
