module Welcome.State
  ( dummyState
  , mkInitialState
  , handleAction
  ) where

import Prelude
import Capability.MainFrameLoop (class MainFrameLoop, callMainFrameAction)
import Capability.Marlowe (class ManageMarlowe, createWallet, lookupWalletDetails)
import Capability.MarloweStorage (class ManageMarloweStorage, clearAllLocalStorage, insertIntoWalletLibrary)
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use, view)
import Data.Map (filter, findMin, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.UUID (toString) as UUID
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, liftEffect, modify_, query, tell)
import Halogen.Extra (mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import Input.Text as TInput
import InputField.State (handleAction, mkInitialState) as InputField
import InputField.Types (Action(..), State) as InputField
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg, welcomeWalletDataIdSlot, welcomeWalletDataNicknameSlot)
import Network.RemoteData (RemoteData(..), fromEither)
import Toast.Types (ajaxErrorToast, errorToast, successToast)
import Types (WebData)
import WalletData.Lenses (_companionAppId, _walletNickname)
import WalletData.State (parsePlutusAppId, parseWalletNickname)
import WalletData.Types (WalletDetails, WalletLibrary)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (location)
import Welcome.Lenses (_card, _cardOpen, _enteringDashboardState, _remoteWalletDetails, _walletId, _walletLibrary, _walletNicknameOrIdInput)
import Welcome.Types (Action(..), Card(..), State, WalletNicknameOrIdError(..))

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = mkInitialState mempty

mkInitialState :: WalletLibrary -> State
mkInitialState walletLibrary =
  { walletLibrary
  , card: Nothing
  , cardOpen: false
  , walletNicknameOrIdInput: InputField.mkInitialState Nothing
  , walletNickname: parseWalletNickname walletLibrary ""
  , walletId: ""
  , remoteWalletDetails: NotAsked
  , enteringDashboardState: false
  }

-- Some actions are handled in `MainFrame.State` because they involve
-- modifications of that state. See Note [State] in MainFrame.State.
handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  MainFrameLoop m =>
  ManageMarlowe m =>
  ManageMarloweStorage m =>
  Toast m =>
  MonadClipboard m =>
  Action -> HalogenM State Action ChildSlots Msg m Unit
handleAction (OpenCard card) =
  modify_
    $ set _card (Just card)
    <<< set _cardOpen true

handleAction CloseCard = do
  modify_
    $ set _remoteWalletDetails NotAsked
    <<< set _enteringDashboardState false
    <<< set _cardOpen false
  handleAction $ WalletNicknameOrIdInputAction $ InputField.Reset
  void
    $ query welcomeWalletDataIdSlot unit
    $ tell TInput.Reset
  void
    $ query welcomeWalletDataNicknameSlot unit
    $ tell TInput.Reset

handleAction GenerateWallet = do
  walletLibrary <- use _walletLibrary
  assign _remoteWalletDetails Loading
  ajaxWalletDetails <- createWallet
  assign _remoteWalletDetails $ fromEither ajaxWalletDetails
  case ajaxWalletDetails of
    Left ajaxError -> addToast $ ajaxErrorToast "Failed to generate wallet." ajaxError
    Right walletDetails -> do
      void
        $ query welcomeWalletDataNicknameSlot unit
        $ tell TInput.Reset
      handleAction $ WalletIdChanged $ UUID.toString (unwrap (view _companionAppId walletDetails))
      handleAction $ OpenCard UseNewWalletCard

handleAction (WalletNicknameOrIdInputAction inputFieldAction) = do
  toWalletNicknameOrIdInput $ InputField.handleAction inputFieldAction
  case inputFieldAction of
    InputField.SetValue walletNicknameOrId -> do
      handleAction $ WalletNicknameOrIdInputAction $ InputField.SetValidator $ const Nothing
      assign _remoteWalletDetails NotAsked
      for_ (parsePlutusAppId walletNicknameOrId) \plutusAppId -> do
        assign _remoteWalletDetails Loading
        handleAction $ WalletNicknameOrIdInputAction $ InputField.SetValidator $ walletNicknameOrIdError Loading
        ajaxWalletDetails <- lookupWalletDetails plutusAppId
        assign _remoteWalletDetails $ fromEither ajaxWalletDetails
        handleAction $ WalletNicknameOrIdInputAction $ InputField.SetValidator $ walletNicknameOrIdError $ fromEither ajaxWalletDetails
        case ajaxWalletDetails of
          Left ajaxError -> pure unit -- feedback is shown in the view in this case
          Right walletDetails -> do
            -- check whether this wallet ID is already in the walletLibrary ...
            walletLibrary <- use _walletLibrary
            case findMin $ filter (\details -> UUID.toString (unwrap (view _companionAppId details)) == walletNicknameOrId) walletLibrary of
              Just { key, value } -> do
                -- if so, open the UseWalletCard
                handleAction $ WalletNicknameChanged $ Right key
                handleAction $ WalletIdChanged walletNicknameOrId
                handleAction $ OpenCard $ UseWalletCard key
              Nothing -> do
                -- otherwise open the UseNewWalletCard
                void
                  $ query welcomeWalletDataNicknameSlot unit
                  $ tell TInput.Reset
                handleAction
                  $ WalletIdChanged
                  $ UUID.toString (unwrap (view _companionAppId walletDetails))
                handleAction $ OpenCard UseNewWalletCard
    InputField.SetValueFromDropdown walletNicknameOrId -> do
      -- in this case we know it's a wallet nickname, and we want to open the use card
      -- for the corresponding wallet
      walletLibrary <- use _walletLibrary
      for_ (lookup walletNicknameOrId walletLibrary) (handleAction <<< OpenUseWalletCardWithDetails)
    _ -> pure unit

handleAction (OpenUseWalletCardWithDetails walletDetails) = do
  assign _remoteWalletDetails Loading
  ajaxWalletDetails <- lookupWalletDetails $ view _companionAppId walletDetails
  assign _remoteWalletDetails $ fromEither ajaxWalletDetails
  case ajaxWalletDetails of
    Left ajaxError -> handleAction $ OpenCard LocalWalletMissingCard
    Right _ -> do
      handleAction $ WalletNicknameOrIdInputAction $ InputField.Reset
      handleAction $ WalletNicknameChanged $ Right $ view _walletNickname walletDetails
      handleAction $ WalletIdChanged $ UUID.toString (unwrap (view _companionAppId walletDetails))
      handleAction $ OpenCard $ UseWalletCard walletDetails.walletNickname

handleAction (WalletNicknameChanged value) = do
  walletLibrary <- use _walletLibrary
  assign _walletNickname value

handleAction (WalletIdChanged value) = do
  walletLibrary <- use _walletLibrary
  assign _walletId value

handleAction (UseWallet walletNickname) = do
  assign _enteringDashboardState true
  remoteWalletDetails <- use _remoteWalletDetails
  case remoteWalletDetails of
    Success walletDetails -> do
      let
        walletDetailsWithNickname = set _walletNickname walletNickname walletDetails
      modifying _walletLibrary (insert walletNickname walletDetailsWithNickname)
      insertIntoWalletLibrary walletDetailsWithNickname
      walletLibrary <- use _walletLibrary
      callMainFrameAction $ MainFrame.EnterDashboardState walletLibrary walletDetailsWithNickname
    _ -> do
      -- this should never happen (the button to use a wallet should be disabled unless
      -- remoteWalletDetails is Success), but let's add some sensible behaviour anyway just in case
      handleAction CloseCard
      addToast $ errorToast "Unable to use this wallet." $ Just "Details for this wallet could not be loaded."

handleAction ClearLocalStorage = do
  clearAllLocalStorage
  liftEffect do
    location_ <- location =<< window
    reload location_

handleAction (ClipboardAction clipboardAction) = do
  mapAction ClipboardAction $ Clipboard.handleAction clipboardAction
  addToast $ successToast "Copied to clipboard"

------------------------------------------------------------
toWalletNicknameOrIdInput ::
  forall m msg slots.
  Functor m =>
  HalogenM (InputField.State WalletNicknameOrIdError) (InputField.Action WalletNicknameOrIdError) slots msg m Unit ->
  HalogenM State Action slots msg m Unit
toWalletNicknameOrIdInput = mapSubmodule _walletNicknameOrIdInput WalletNicknameOrIdInputAction

------------------------------------------------------------
walletNicknameOrIdError :: WebData WalletDetails -> String -> Maybe WalletNicknameOrIdError
walletNicknameOrIdError remoteWalletDetails _ = case remoteWalletDetails of
  Loading -> Just UnconfirmedWalletNicknameOrId
  Failure _ -> Just NonexistentWalletNicknameOrId
  _ -> Nothing
