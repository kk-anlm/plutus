module Template.State
  ( dummyState
  , initialState
  , handleAction
  , instantiateExtendedContract
  , parseTemplateForm
  , parseContractNickname
  ) where

import Prelude
import Control.Monad.Reader (class MonadAsk)
import Data.Array (mapMaybe) as Array
import Data.BigInteger (BigInteger)
import Data.Either (Either(..))
import Data.Lens (Lens', _Left, _Right, assign, preview, set, use, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (Maybe(..), maybe)
import Data.Set (toUnfoldable) as Set
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
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
import InputField.State (formatBigIntegerValue, getBigIntegerValue)
import InputField.Types (Action(..), State) as InputField
import InputField.Types (class InputFieldError)
import MainFrame.Types (ChildSlots, Msg, dashboardTemplateNicknameSlot)
import Marlowe.Extended (Contract) as Extended
import Marlowe.Extended (ContractType(..), resolveRelativeTimes, toCore)
import Marlowe.Extended.Metadata (MetaData, NumberFormat(..), _valueParameterFormat, _valueParameterInfo)
import Marlowe.HasParties (getParties)
import Marlowe.Semantics (Contract) as Semantic
import Marlowe.Semantics (Party(..), Slot, TokenName)
import Marlowe.Template (TemplateContent(..), _slotContent, _valueContent, fillTemplate, getPlaceholderIds, initializeTemplateContent)
import Template.Lenses (_contractNickname, _contractSetupStage, _contractTemplate, _roleWalletInput, _roleWalletInputs, _slotContentInput, _slotContentInputs, _valueContentInput, _valueContentInputs)
import Template.Types (Action(..), ContractNicknameError(..), ContractSetupStage(..), Input, RoleError(..), SlotError(..), State, TemplateFormResult(..), ValueError(..))
import WalletData.Types (WalletLibrary)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = initialState

initialState :: State
initialState =
  { contractSetupStage: Start
  , contractTemplate: Escrow.contractTemplate
  , contractNickname: parseContractNickname ""
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
    <<< query dashboardTemplateNicknameSlot unit
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
  setInputValidators input _valueContentInputs ValueContentInputAction (parseValue DefaultFormat)
  setInputValidators input _slotContentInputs SlotContentInputAction parseSlot

handleAction _ (OpenCreateWalletCard tokenName) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction _ (ContractNicknameChanged value) = assign _contractNickname value

handleAction input@{ walletLibrary } UpdateRoleWalletValidators = setInputValidators input _roleWalletInputs RoleWalletInputAction $ parseRole walletLibrary

handleAction _ (RoleWalletInputAction tokenName inputFieldAction) = toRoleWalletInput tokenName $ InputField.handleAction inputFieldAction

handleAction _ (SlotContentInputAction key inputFieldAction) = toSlotContentInput key $ InputField.handleAction inputFieldAction

handleAction _ (ValueContentInputAction key inputFieldAction) = toValueContentInput key $ InputField.handleAction inputFieldAction

handleAction _ (StartContract _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

setInputValidators ::
  forall e a m.
  MonadAff m =>
  MonadAsk Env m =>
  InputFieldError e =>
  Input ->
  Lens' State (Map String (InputField.State e)) ->
  (String -> (InputField.Action e -> Action)) ->
  (String -> Either e a) ->
  HalogenM State Action ChildSlots Msg m Unit
setInputValidators input lens action validator = do
  inputFields <- use lens
  let
    (inputFieldKeys :: Array String) = Set.toUnfoldable $ Map.keys inputFields
  void
    $ for inputFieldKeys \key ->
        handleAction input $ action key $ InputField.SetValidator $ preview _Left <<< validator

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

instantiateExtendedContract :: Slot -> Extended.Contract -> TemplateContent -> Maybe Semantic.Contract
instantiateExtendedContract currentSlot extendedContract content =
  let
    filledContract = fillTemplate content extendedContract

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
parseContractNickname :: String -> Either ContractNicknameError String
parseContractNickname "" = Left EmptyContractNickname

parseContractNickname v = Right v

parseRole :: WalletLibrary -> String -> Either RoleError String
parseRole _ "" = Left EmptyNickname

parseRole walletLibrary walletNickname =
  if Map.member walletNickname walletLibrary then
    Right walletNickname
  else
    Left NonExistentNickname

-- TODO: Add proper slot input validation. It's not necessary yet, because slot parameters are
-- readonly for now.
parseSlot :: String -> Either SlotError BigInteger
parseSlot "" = Left EmptySlot

parseSlot s = Right $ getBigIntegerValue TimeFormat s

parseValue :: NumberFormat -> String -> Either ValueError BigInteger
parseValue _ "" = Left EmptyValue

parseValue format v = Right $ getBigIntegerValue format v

parseTemplateForm :: WalletLibrary -> State -> Maybe TemplateFormResult
parseTemplateForm walletLibrary state =
  TemplateFormResult
    <$> preview _Right state.contractNickname
    <*> traverse
        (preview _Right <<< parseRole walletLibrary <<< _.value)
        state.roleWalletInputs
    <*> templateContent
  where
  templateContent = do
    slotContent <-
      traverse
        (preview _Right <<< parseSlot <<< _.value)
        state.slotContentInputs
    valueContent <-
      traverseWithIndex
        (\key { value } -> preview _Right $ parseValue (getFormatFor key) value)
        state.valueContentInputs
    pure $ TemplateContent { slotContent, valueContent }

  getFormatFor key =
    maybe
      DefaultFormat
      _.valueParameterFormat
      $ OMap.lookup key state.contractTemplate.metaData.valueParameterInfo
