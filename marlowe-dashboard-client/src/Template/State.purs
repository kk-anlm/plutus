module Template.State
  ( dummyState
  , initialState
  , handleAction
  , instantiateExtendedContract
  , templateSetupIsValid
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Array (mapMaybe) as Array
import Data.BigInteger (BigInteger)
import Data.Lens (Lens', assign, set, use, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (Maybe(..), isNothing)
import Data.Set (toUnfoldable) as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Examples.PureScript.ContractForDifferences (defaultSlotContent) as ContractForDifferences
import Examples.PureScript.Escrow (contractTemplate, defaultSlotContent) as Escrow
import Examples.PureScript.EscrowWithCollateral (defaultSlotContent) as EscrowWithCollateral
import Examples.PureScript.Swap (defaultSlotContent) as Swap
import Examples.PureScript.ZeroCouponBond (defaultSlotContent) as ZeroCouponBond
import Halogen (HalogenM, Tell, modify_, query, tell)
import Halogen.Extra (mapMaybeSubmodule)
import Input.Text as TInput
import InputField.Lenses (_value)
import InputField.State (dummyState, handleAction, mkInitialState) as InputField
import InputField.State (formatBigIntegerValue, getBigIntegerValue, validate)
import InputField.Types (Action(..), State) as InputField
import InputField.Types (class InputFieldError)
import MainFrame.Types (ChildSlots, Msg, contractTemplateInputSlot)
import Marlowe.Extended (Contract) as Extended
import Marlowe.Extended (ContractType(..), resolveRelativeTimes, toCore)
import Marlowe.Extended.Metadata (MetaData, NumberFormat(..), _extendedContract, _metaData, _valueParameterFormat, _valueParameterInfo)
import Marlowe.HasParties (getParties)
import Marlowe.Semantics (Contract) as Semantic
import Marlowe.Semantics (Party(..), Slot, TokenName)
import Marlowe.Template (TemplateContent(..), _slotContent, _valueContent, fillTemplate, getPlaceholderIds, initializeTemplateContent)
import Template.Lenses (_contractNickname, _contractNicknameError, _contractSetupStage, _contractTemplate, _roleWalletInput, _roleWalletInputs, _slotContentInput, _slotContentInputs, _valueContentInput, _valueContentInputs)
import Template.Types (Action(..), ContractNicknameError(..), ContractSetupStage(..), Input, InputSlot(..), RoleError(..), SlotError(..), State, ValueError(..))
import WalletData.Types (WalletLibrary)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = initialState

initialState :: State
initialState =
  let
    templateContent = initializeTemplateContent $ getPlaceholderIds Escrow.contractTemplate.extendedContract
  in
    { contractSetupStage: Start
    , contractTemplate: Escrow.contractTemplate
    , contractNickname: ""
    , contractNicknameError: Nothing
    , roleWalletInputs: mempty
    , slotContentInputs: mempty
    , valueContentInputs: mempty
    }

tellNicknameInputTo ::
  forall m.
  Tell TInput.Query ->
  HalogenM State Action ChildSlots Msg m Unit
tellNicknameInputTo =
  void
    <<< query TInput.label (contractTemplateInputSlot ContractNicknameInput)
    <<< tell

-- Some actions are handled in `Dashboard.State` because they involve
-- modifications of that state. See Note [State] in MainFrame.State.
handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  Input ->
  Action ->
  HalogenM State Action ChildSlots Msg m Unit
handleAction _ (SetContractSetupStage contractSetupStage) = do
  assign _contractSetupStage contractSetupStage
  when (contractSetupStage == Setup) $ tellNicknameInputTo TInput.Focus

handleAction input@{ currentSlot } (SetTemplate contractTemplate) = do
  let
    templateContent = initializeTemplateContent $ getPlaceholderIds contractTemplate.extendedContract

    slotContent = view _slotContent templateContent

    valueContent = view _valueContent templateContent

    roleWalletInputs = mkRoleWalletInputs contractTemplate.extendedContract

    slotContentInputs = mkSlotContentInputs contractTemplate.metaData slotContent

    valueContentInputs = mkValueContentInputs contractTemplate.metaData valueContent
  modify_
    $ set _contractSetupStage Overview
    <<< set _contractTemplate contractTemplate
    <<< set _roleWalletInputs roleWalletInputs
    <<< set _slotContentInputs slotContentInputs
    <<< set _valueContentInputs valueContentInputs
  tellNicknameInputTo TInput.Reset
  handleAction input UpdateRoleWalletValidators
  setInputValidators input _valueContentInputs ValueContentInputAction valueError
  setInputValidators input _slotContentInputs SlotContentInputAction slotError

handleAction _ (OpenCreateWalletCard tokenName) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction _ (ContractNicknameInputChanged value) = do
  assign _contractNickname value
  assign _contractNicknameError $ contractNicknameError value

handleAction input@{ walletLibrary } UpdateRoleWalletValidators = setInputValidators input _roleWalletInputs RoleWalletInputAction $ roleError walletLibrary

handleAction _ (RoleWalletInputAction tokenName inputFieldAction) = toRoleWalletInput tokenName $ InputField.handleAction inputFieldAction

handleAction _ (SlotContentInputAction key inputFieldAction) = toSlotContentInput key $ InputField.handleAction inputFieldAction

handleAction _ (ValueContentInputAction key inputFieldAction) = toValueContentInput key $ InputField.handleAction inputFieldAction

handleAction _ StartContract = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

setInputValidators ::
  forall e m.
  MonadAff m =>
  MonadAsk Env m =>
  InputFieldError e =>
  Input ->
  Lens' State (Map String (InputField.State e)) ->
  (String -> (InputField.Action e -> Action)) ->
  (String -> Maybe e) ->
  HalogenM State Action ChildSlots Msg m Unit
setInputValidators input lens action validator = do
  inputFields <- use lens
  let
    (inputFieldKeys :: Array String) = Set.toUnfoldable $ Map.keys inputFields
  void
    $ for inputFieldKeys \key ->
        handleAction input $ action key $ InputField.SetValidator validator

------------------------------------------------------------
mkRoleWalletInputs :: Extended.Contract -> Map TokenName (InputField.State RoleError)
mkRoleWalletInputs contract = Map.fromFoldable $ Array.mapMaybe getRoleInput (Set.toUnfoldable $ getParties contract)
  where
  getRoleInput :: Party -> Maybe (Tuple TokenName (InputField.State RoleError))
  getRoleInput (PK pubKey) = Nothing

  getRoleInput (Role tokenName) = Just (Tuple tokenName $ InputField.mkInitialState Nothing)

mkSlotContentInputs :: MetaData -> Map String BigInteger -> Map String (InputField.State SlotError)
mkSlotContentInputs metaData slotContent =
  let
    defaultSlotContent = case metaData.contractType of
      Escrow -> Escrow.defaultSlotContent
      EscrowWithCollateral -> EscrowWithCollateral.defaultSlotContent
      Swap -> Swap.defaultSlotContent
      ZeroCouponBond -> ZeroCouponBond.defaultSlotContent
      ContractForDifferences -> ContractForDifferences.defaultSlotContent
      _ -> mempty

    mkSlotContentInput key _ =
      let
        inputFieldInitialState = InputField.mkInitialState $ Just DefaultFormat
      in
        case Map.lookup key defaultSlotContent of
          Just value -> Just $ set _value (formatBigIntegerValue TimeFormat value) inputFieldInitialState
          Nothing -> Just inputFieldInitialState
  in
    Map.mapMaybeWithKey mkSlotContentInput slotContent

mkValueContentInputs :: MetaData -> Map String BigInteger -> Map String (InputField.State ValueError)
mkValueContentInputs metaData valueContent = Map.mapMaybeWithKey valueToInput valueContent
  where
  valueToInput key value = case OMap.lookup key $ map (view _valueParameterFormat) (view _valueParameterInfo metaData) of
    Just numberFormat -> Just $ InputField.mkInitialState $ Just numberFormat
    _ -> Just $ InputField.mkInitialState Nothing

instantiateExtendedContract :: Slot -> State -> Maybe Semantic.Contract
instantiateExtendedContract currentSlot state =
  let
    extendedContract = view (_contractTemplate <<< _extendedContract) state

    slotContentInputs = view _slotContentInputs state

    valueContentInputs = view _valueContentInputs state

    slotContent = map (getBigIntegerValue TimeFormat <<< view _value) slotContentInputs

    valueParameterFormats = map (view _valueParameterFormat) (view (_contractTemplate <<< _metaData <<< _valueParameterInfo) state)

    getBigIntegerValueWithDecimals key valueContentInput = case OMap.lookup key valueParameterFormats of
      Just numberFormat -> Just $ getBigIntegerValue numberFormat $ view _value valueContentInput
      _ -> Just $ getBigIntegerValue DefaultFormat $ view _value valueContentInput

    valueContent = Map.mapMaybeWithKey getBigIntegerValueWithDecimals valueContentInputs

    templateContent = TemplateContent { slotContent, valueContent }

    filledContract = fillTemplate templateContent extendedContract

    absoluteFilledContract = resolveRelativeTimes currentSlot filledContract
  in
    toCore absoluteFilledContract

------------------------------------------------------------
toRoleWalletInput ::
  forall m msg slots.
  Functor m =>
  TokenName ->
  HalogenM (InputField.State RoleError) (InputField.Action RoleError) slots msg m Unit ->
  HalogenM State Action slots msg m Unit
toRoleWalletInput tokenName = mapMaybeSubmodule (_roleWalletInput tokenName) (RoleWalletInputAction tokenName) InputField.dummyState

toSlotContentInput ::
  forall m msg slots.
  Functor m =>
  String ->
  HalogenM (InputField.State SlotError) (InputField.Action SlotError) slots msg m Unit ->
  HalogenM State Action slots msg m Unit
toSlotContentInput key = mapMaybeSubmodule (_slotContentInput key) (SlotContentInputAction key) InputField.dummyState

toValueContentInput ::
  forall m msg slots.
  Functor m =>
  String ->
  HalogenM (InputField.State ValueError) (InputField.Action ValueError) slots msg m Unit ->
  HalogenM State Action slots msg m Unit
toValueContentInput key = mapMaybeSubmodule (_valueContentInput key) (ValueContentInputAction key) InputField.dummyState

------------------------------------------------------------
contractNicknameError :: String -> Maybe ContractNicknameError
contractNicknameError "" = Just EmptyContractNickname

contractNicknameError _ = Nothing

roleError :: WalletLibrary -> String -> Maybe RoleError
roleError _ "" = Just EmptyNickname

roleError walletLibrary walletNickname =
  if Map.member walletNickname walletLibrary then
    Nothing
  else
    Just NonExistentNickname

-- TODO: Add proper slot input validation. It's not necessary yet, because slot parameters are
-- readonly for now.
slotError :: String -> Maybe SlotError
slotError "" = Just EmptySlot

slotError _ = Nothing

valueError :: String -> Maybe ValueError
valueError "" = Just EmptyValue

valueError _ = Nothing

templateSetupIsValid :: State -> Boolean
templateSetupIsValid state =
  let
    roleWalletInputs = view _roleWalletInputs state

    slotContentInputs = view _slotContentInputs state

    valueContentInputs = view _valueContentInputs state
  in
    (isNothing $ state.contractNicknameError)
      && (Map.isEmpty $ Map.mapMaybe validate roleWalletInputs)
      && (Map.isEmpty $ Map.mapMaybe validate slotContentInputs)
      && (Map.isEmpty $ Map.mapMaybe validate valueContentInputs)
