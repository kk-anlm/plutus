module WalletData.State
  ( mkInitialState
  , defaultWalletDetails
  , handleAction
  , adaToken
  , getAda
  , walletNicknameError
  , walletIdError
  , parsePlutusAppId
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
import Dashboard.Types (InputSlot(..))
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
import Halogen.Extra (mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import Input.Text as TInput
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, InputSlot(..), Msg, walletDataInputSlot)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Semantics (Assets, Token(..))
import Network.RemoteData (RemoteData(..), fromEither)
import Toast.Types (errorToast, successToast)
import Types (WebData)
import WalletData.Lenses (_cardSection, _remoteWalletInfo, _walletId, _walletIdError, _walletLibrary, _walletNickname, _walletNicknameError)
import WalletData.Types (Action(..), CardSection(..), InputSlot(..), PubKeyHash(..), State, Wallet(..), WalletDetails, WalletIdError(..), WalletInfo(..), WalletLibrary, WalletNickname, WalletNicknameError(..))

mkInitialState :: WalletLibrary -> State
mkInitialState walletLibrary =
  { walletLibrary
  , cardSection: Home
  , walletNickname: ""
  , walletNicknameError: Nothing
  , walletId: ""
  , walletIdError: Nothing
  , remoteWalletInfo: NotAsked
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
        $ query TInput.label (walletDataInputSlot WalletIdInput)
        $ tell TInput.Reset
      void
        $ query TInput.label (walletDataInputSlot WalletNicknameInput)
        $ tell TInput.Reset
    _ -> pure unit
  assign _cardSection cardSection

handleAction (SaveWallet mTokenName) = do
  oldWalletLibrary <- use _walletLibrary
  walletNickname <- use _walletNickname
  walletIdString <- use _walletId
  remoteWalletInfo <- use _remoteWalletInfo
  let
    mWalletId = parsePlutusAppId walletIdString
  case remoteWalletInfo, mWalletId of
    Success walletInfo, Just walletId -> do
      let
        -- note the empty properties are fine for saved wallets - these will be fetched if/when
        -- this wallet is picked up
        walletDetails =
          { walletNickname
          , companionAppId: walletId
          , marloweAppId: PlutusAppId emptyUUID
          , walletInfo
          , assets: mempty
          , previousCompanionAppState: Nothing
          }
      modifying _walletLibrary (insert walletNickname walletDetails)
      insertIntoWalletLibrary walletDetails
      newWalletLibrary <- use _walletLibrary
      -- if a tokenName was also passed, we need to update the contract setup data
      for_ mTokenName \tokenName -> callMainFrameAction $ MainFrame.DashboardAction $ Dashboard.SetContactForRole tokenName walletNickname
    -- TODO: show error feedback to the user (just to be safe - but this should never happen, because
    -- the button to save a new wallet should be disabled in this case)
    _, _ -> pure unit

handleAction CancelNewContactForRole = pure unit -- handled in Dashboard.State

handleAction (WalletNicknameChanged value) = do
  walletLibrary <- use _walletLibrary
  assign _walletNickname value
  assign _walletNicknameError $ walletNicknameError walletLibrary value

handleAction (WalletIdChanged value) = do
  -- note we assign the value _first_ so that the InputField value is set - otherwise the
  -- validation feedback is wrong while the rest is happening
  assign _walletId value
  handleAction $ SetRemoteWalletInfo NotAsked
  -- if this is a valid contract ID ...
  for_ (parsePlutusAppId value) \walletId -> do
    handleAction $ SetRemoteWalletInfo Loading
    -- .. lookup wallet info
    ajaxWalletInfo <- lookupWalletInfo walletId
    handleAction $ SetRemoteWalletInfo $ fromEither ajaxWalletInfo

handleAction (SetRemoteWalletInfo remoteWalletInfo) = do
  assign _remoteWalletInfo remoteWalletInfo
  walletLibrary <- use _walletLibrary
  walletId <- use _walletId
  assign _walletIdError $ walletIdError remoteWalletInfo walletLibrary walletId

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

walletNicknameError :: WalletLibrary -> WalletNickname -> Maybe WalletNicknameError
walletNicknameError _ "" = Just EmptyWalletNickname

walletNicknameError walletLibrary walletNickname =
  if member walletNickname walletLibrary then
    Just DuplicateWalletNickname
  else
    if any (\char -> not $ isAlphaNum char) $ toCharArray walletNickname then
      Just BadWalletNickname
    else
      Nothing

walletIdError :: WebData WalletInfo -> WalletLibrary -> String -> Maybe WalletIdError
walletIdError _ _ "" = Just EmptyWalletId

walletIdError remoteDataWalletInfo walletLibrary walletIdString = case parsePlutusAppId walletIdString of
  Nothing -> Just InvalidWalletId
  Just plutusAppId
    | not $ isEmpty $ filter (\walletDetails -> walletDetails.companionAppId == plutusAppId) walletLibrary -> Just DuplicateWalletId
  _ -> case remoteDataWalletInfo of
    Success _ -> Nothing
    Failure _ -> Just NonexistentWalletId
    _ -> Just UnconfirmedWalletId

parsePlutusAppId :: String -> Maybe PlutusAppId
parsePlutusAppId plutusAppIdString = case parseUUID plutusAppIdString of
  Just uuid -> Just $ PlutusAppId uuid
  Nothing -> Nothing
