module Template.Types
  ( State
  , ContractSetupStage(..)
  , Input
  , ContractNicknameError(..)
  , RoleError(..)
  , SlotError(..)
  , ValueError(..)
  , Action(..)
  , TemplateFormResult(..)
  ) where

import Prelude
import Analytics (class IsEvent, defaultEvent, toEvent)
import Data.Display (class Display)
import Data.Either (Either)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import InputField.Types (Action, State) as InputField
import InputField.Types (class InputFieldError)
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.Semantics (Slot, TokenName)
import Marlowe.Template (TemplateContent)
import WalletData.Types (WalletLibrary)

-- TODO parse to better types than String
data TemplateFormResult
  = TemplateFormResult String (Map String String) TemplateContent

derive instance eqTemplateFormResult :: Eq TemplateFormResult

type State
  = { contractSetupStage :: ContractSetupStage
    , contractTemplate :: ContractTemplate
    , contractNickname :: Either ContractNicknameError String
    , roleWalletInputs :: Map TokenName (InputField.State RoleError)
    , slotContentInputs :: Map String (InputField.State SlotError)
    , valueContentInputs :: Map String (InputField.State ValueError)
    }

data ContractSetupStage
  = Start
  | Overview
  | Setup
  | Review TemplateFormResult

derive instance eqContractSetupStage :: Eq ContractSetupStage

type Input
  = { currentSlot :: Slot
    , walletLibrary :: WalletLibrary
    }

data ContractNicknameError
  = EmptyContractNickname

derive instance eqContractNicknameError :: Eq ContractNicknameError

instance inputFieldErrorContractNicknameError :: Display ContractNicknameError where
  display EmptyContractNickname = "Contract nickname cannot be blank"

data RoleError
  = EmptyNickname
  | NonExistentNickname

derive instance eqRoleError :: Eq RoleError

instance inputFieldErrorRoleError :: InputFieldError RoleError where
  inputErrorToString EmptyNickname = "Role nickname cannot be blank"
  inputErrorToString NonExistentNickname = "Nickname not found in your wallet library"

data SlotError
  = EmptySlot
  | NegativeSlot
  | BadDateTimeString

derive instance eqSlotError :: Eq SlotError

instance inputFieldErrorSlotError :: InputFieldError SlotError where
  inputErrorToString EmptySlot = "Timeout cannot be blank"
  inputErrorToString NegativeSlot = "Timeout cannot be negative"
  inputErrorToString BadDateTimeString = "Invalid timeout"

data ValueError
  = EmptyValue

derive instance eqValueError :: Eq ValueError

instance inputFieldErrorValueError :: InputFieldError ValueError where
  inputErrorToString EmptyValue = "Value cannot be blank"

data Action
  = SetContractSetupStage ContractSetupStage
  | SetTemplate ContractTemplate
  | OpenCreateWalletCard TokenName
  | ContractNicknameChanged (Either ContractNicknameError String)
  | UpdateRoleWalletValidators
  | RoleWalletInputAction TokenName (InputField.Action RoleError)
  | SlotContentInputAction String (InputField.Action SlotError)
  | ValueContentInputAction String (InputField.Action ValueError)
  | StartContract TemplateFormResult

-- | Here we decide which top-level queries to track as GA events, and
-- how to classify them.
instance actionIsEvent :: IsEvent Action where
  toEvent (SetContractSetupStage _) = Just $ defaultEvent "SetContractSetupStage"
  toEvent (SetTemplate _) = Just $ defaultEvent "SetTemplate"
  toEvent (OpenCreateWalletCard _) = Nothing
  toEvent (ContractNicknameChanged _) = Nothing
  toEvent UpdateRoleWalletValidators = Nothing
  toEvent (RoleWalletInputAction _ inputFieldAction) = toEvent inputFieldAction
  toEvent (SlotContentInputAction _ inputFieldAction) = toEvent inputFieldAction
  toEvent (ValueContentInputAction _ inputFieldAction) = toEvent inputFieldAction
  toEvent (StartContract _) = Just $ defaultEvent "StartContract"
