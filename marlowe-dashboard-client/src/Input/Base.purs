module Input.Base
  ( Action
  , HTML
  , Message(..)
  , Props
  , Query(..)
  , Slot
  , State
  , defaultProps
  , mkInputProps
  , component
  ) where

import Prelude
import Control.MonadPlus (guard)
import Data.Lens (Lens', assign, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Extra (blur, focus)
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onValueInput_)
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.HTML (HTMLElement)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------
type Props msg slots m
  = { render :: State -> HTML msg slots m
    , value :: String
    , error :: Maybe String
    }

data Message msg
  = ValueChanged String
  | Emitted msg

data Query a
  = Focus a
  | Blur a
  | SetText String a
  | GetState (State -> a)
  | GetInputElement (HTMLElement -> a)

data Action msg slots m
  = PropsReceived (Props msg slots m)
  | Changed String
  | Raise msg

type InnerState msg slots m
  = { props :: Props msg slots m
    , pristine :: Boolean
    }

defaultProps :: forall msg slots m. Props msg slots m
defaultProps =
  { render: const $ HH.text ""
  , value: ""
  , error: Nothing
  }

initialState :: forall msg slots m. Props msg slots m -> InnerState msg slots m
initialState props =
  { props
  , pristine: true
  }

type State
  = { value :: String
    , error :: Maybe String
    }

innerStateToState :: forall msg slots m. InnerState msg slots m -> State
innerStateToState { props: { value, error }, pristine } =
  { value
  , error: guard (not pristine) *> error
  }

type HTML msg slots m
  = H.ComponentHTML (Action msg slots m) slots m

type InputM msg slots m
  = H.HalogenM
      (InnerState msg slots m)
      (Action msg slots m)
      slots
      (Message msg)
      m

type Slot msg id
  = H.Slot Query (Message msg) id

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------
_render :: forall a r. Lens' { render :: a | r } a
_render = prop (SProxy :: SProxy "render")

_value :: forall a r. Lens' { value :: a | r } a
_value = prop (SProxy :: SProxy "value")

_error :: forall a r. Lens' { error :: a | r } a
_error = prop (SProxy :: SProxy "error")

_props :: forall a r. Lens' { props :: a | r } a
_props = prop (SProxy :: SProxy "props")

_pristine :: forall a r. Lens' { pristine :: a | r } a
_pristine = prop (SProxy :: SProxy "pristine")

_stateValue :: forall msg slots m. Lens' (InnerState msg slots m) String
_stateValue = _props <<< _value

-------------------------------------------------------------------------------
-- Render helpers
-------------------------------------------------------------------------------
type InputProps r
  = ( value :: String
    , onInput :: Event
    | r
    )

inputRef :: H.RefLabel
inputRef = H.RefLabel "input"

mkInputProps ::
  forall msg slots m r.
  String ->
  Array (HH.IProp (InputProps r) (Action msg slots m)) ->
  Array (HH.IProp (InputProps r) (Action msg slots m))
mkInputProps value overrides =
  [ HP.ref inputRef
  , HP.value value
  , onValueInput_ Changed
  ]
    <> overrides

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
component ::
  forall msg slots m.
  MonadAff m =>
  H.Component HH.HTML Query (Props msg slots m) (Message msg) m
component =
  H.mkComponent
    { initialState
    , render: \state -> state.props.render $ innerStateToState state
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< PropsReceived
            }
    }

handleAction ::
  forall msg slots m.
  MonadAff m =>
  Action msg slots m ->
  InputM msg slots m Unit
handleAction = case _ of
  PropsReceived props -> H.modify_ _ { props = props }
  Changed value -> do
    assign _stateValue value
    assign _pristine false
    H.raise $ ValueChanged value
  Raise msg -> H.raise $ Emitted msg

handleQuery ::
  forall msg slots m a.
  MonadAff m =>
  Query a ->
  InputM msg slots m (Maybe a)
handleQuery = case _ of
  Focus n -> do
    focus inputRef
    pure $ Just n
  Blur n -> do
    blur inputRef
    pure $ Just n
  SetText value n -> do
    currentValue <- use _stateValue
    when (currentValue /= value) do
      assign _stateValue value
      H.raise $ ValueChanged value
    pure $ Just n
  GetState q -> do
    (Just <<< q <<< innerStateToState) <$> H.get
  GetInputElement q -> do
    map q <$> H.getHTMLElementRef inputRef

raise :: forall msg slots m. msg -> Action msg slots m
raise = Raise
