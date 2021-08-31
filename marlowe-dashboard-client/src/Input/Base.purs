module Input.Base
  ( Input
  , Output(..)
  , State
  , Action(..)
  , defaultInput
  , handleAction
  , initialState
  , inputProps
  , renderError
  ) where

import Prelude
import Control.MonadPlus (guard)
import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType)
import Data.Array (catMaybes)
import Data.Lens (Lens', assign)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onValueInput_)
import Halogen.HTML.Properties as HP

-------------------------------------------------------------------------------
-- Input
-------------------------------------------------------------------------------
-- | Input fields shared by all input component types.
type Input r
  = { additionalCss :: Array String
    , id_ :: Maybe String
    , readOnly :: Boolean
    , value :: String
    , error :: Maybe String
    | r
    }

_additionalCss :: forall a r. Lens' { additionalCss :: a | r } a
_additionalCss = prop (SProxy :: SProxy "additionalCss")

_id_ :: forall a r. Lens' { id_ :: a | r } a
_id_ = prop (SProxy :: SProxy "id_")

_readOnly :: forall a r. Lens' { readOnly :: a | r } a
_readOnly = prop (SProxy :: SProxy "readOnly")

_value :: forall a r. Lens' { value :: a | r } a
_value = prop (SProxy :: SProxy "value")

_error :: forall a r. Lens' { error :: a | r } a
_error = prop (SProxy :: SProxy "error")

-- | A default input value
defaultInput :: Input ()
defaultInput =
  { additionalCss: []
  , id_: Nothing
  , readOnly: false
  , value: ""
  , error: Nothing
  }

-------------------------------------------------------------------------------
-- Output
-------------------------------------------------------------------------------
-- | Output events that can be observed on an input
data Output
  = ValueChanged String
  | Focused
  | Blurred

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------
-- | The internal state of a text input
type State ri r
  = { input :: Input ri
    , pristine :: Boolean
    | r
    }

_input :: forall a r. Lens' { input :: a | r } a
_input = prop (SProxy :: SProxy "input")

_pristine :: forall a r. Lens' { pristine :: a | r } a
_pristine = prop (SProxy :: SProxy "pristine")

-- | Initialize the state given input
initialState :: forall r. Input r -> State r ()
initialState input = { input, pristine: true }

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------
data Action r
  = ValueChangedInternal String
  | FocusedInternal
  | BlurredInternal
  | InputReceived (Input r)

-------------------------------------------------------------------------------
-- handleAction
-------------------------------------------------------------------------------
handleAction ::
  forall m slots ri r.
  Monad m =>
  Action ri ->
  H.HalogenM (State ri r) (Action ri) slots Output m Unit
handleAction (ValueChangedInternal value) = do
  assign (_input <<< _value) value
  assign _pristine false
  H.raise $ ValueChanged value

handleAction FocusedInternal = do
  H.raise Focused

handleAction BlurredInternal = do
  H.raise Blurred

handleAction (InputReceived input) = do
  assign _input input

-------------------------------------------------------------------------------
-- Render helpers
-------------------------------------------------------------------------------
inputProps ::
  forall ri r.
  InputType ->
  (Boolean -> Array String) ->
  State ri r ->
  Array (HP.IProp HTMLinput (Action ri))
inputProps inputType inputStyle state@{ input } =
  catMaybes
    [ Just $ HP.type_ inputType
    , Just $ classNames $ inputStyle (isNothing $ visibleError state) <> input.additionalCss
    , HP.id_ <$> input.id_
    , Just $ HP.value input.value
    , Just $ HP.readOnly input.readOnly
    , Just $ onValueInput_ ValueChangedInternal
    ]

renderError :: forall w ri r. State ri r -> HH.HTML w (Action ri)
renderError state =
  HH.div
    [ classNames Css.inputError ]
    [ HH.text $ fromMaybe "" $ visibleError state ]

visibleError :: forall ri r. State ri r -> Maybe String
visibleError { input: { error }, pristine } = guard pristine *> error
