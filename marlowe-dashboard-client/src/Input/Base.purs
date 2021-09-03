module Input.Base
  ( Action
  , Component
  , HTML
  , Message(..)
  , Props
  , Query(..)
  , Slot
  , State
  , component
  , defaultHandleMessage
  , mkInputProps
  , renderError
  ) where

import Prelude
import Control.MonadPlus (guard)
import Css as Css
import Data.Display (class Display, display)
import Data.Either (Either(..), either)
import Data.Lens (Lens', assign, use)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (modify_)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Extra (focus)
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onValueInput_)
import Halogen.HTML.Extra (maybeHTML)
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------
type Props msg slots m e a
  = { render :: State -> HTML msg slots m e a
    , value :: Either e a
    , parse :: String -> Either e a
    }

data Message msg e a
  = ValueChanged (Either e a)
  | Emitted msg

defaultHandleMessage :: forall n e a. (Either e a -> n) -> Message Void e a -> n
defaultHandleMessage f (ValueChanged v) = f v

defaultHandleMessage _ (Emitted m) = absurd m

data Query a
  = Focus a
  | Reset a

data Action msg slots m e a
  = PropsReceived (Props msg slots m e a)
  | Changed String
  | Raise msg

type InnerState msg slots m e a
  = { error :: Maybe String
    , pristine :: Boolean
    , props :: Props msg slots m e a
    , displayedValue :: String
    }

initialState ::
  forall msg slots m e a.
  Display a =>
  Props msg slots m e a ->
  InnerState msg slots m e a
initialState props =
  { props
  , pristine: true
  , error: Nothing
  , displayedValue: either (const "") display props.value
  }

type State
  = { value :: String
    , error :: Maybe String
    }

innerStateToState ::
  forall msg slots m e a.
  Display e =>
  InnerState msg slots m e a ->
  State
innerStateToState { props: { value }, displayedValue, pristine } =
  { value: displayedValue
  , error: guard (not pristine) *> errorText value
  }
  where
  errorText (Left e) = Just $ display e

  errorText _ = Nothing

type HTML msg slots m e a
  = H.ComponentHTML (Action msg slots m e a) slots m

type InputM msg slots m e a
  = H.HalogenM
      (InnerState msg slots m e a)
      (Action msg slots m e a)
      slots
      (Message msg e a)
      m

type Slot msg e a
  = H.Slot Query (Message msg e a)

type Component msg slots m e a
  = H.Component HH.HTML Query (Props msg slots m e a) (Message msg e a) m

-------------------------------------------------------------------------------
-- Optics
-------------------------------------------------------------------------------
_render :: forall a r. Lens' { render :: a | r } a
_render = prop (SProxy :: SProxy "render")

_parse :: forall a r. Lens' { parse :: a | r } a
_parse = prop (SProxy :: SProxy "parse")

_displayedValue :: forall a r. Lens' { displayedValue :: a | r } a
_displayedValue = prop (SProxy :: SProxy "displayedValue")

_error :: forall a r. Lens' { error :: a | r } a
_error = prop (SProxy :: SProxy "error")

_props :: forall a r. Lens' { props :: a | r } a
_props = prop (SProxy :: SProxy "props")

_pristine :: forall a r. Lens' { pristine :: a | r } a
_pristine = prop (SProxy :: SProxy "pristine")

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
  forall msg slots m e a r.
  String ->
  Array (HH.IProp (InputProps r) (Action msg slots m e a)) ->
  Array (HH.IProp (InputProps r) (Action msg slots m e a))
mkInputProps value overrides =
  [ HP.ref inputRef
  , HP.value value
  , onValueInput_ Changed
  ]
    <> overrides

renderError :: forall p i. Maybe String -> HH.HTML p i
renderError error =
  HH.div
    [ classNames Css.inputError ]
    [ maybeHTML HH.text error ]

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
component ::
  forall msg slots m e a.
  MonadAff m =>
  Display e =>
  Display a =>
  Component msg slots m e a
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
  forall msg slots m e a.
  MonadAff m =>
  Action msg slots m e a ->
  InputM msg slots m e a Unit
handleAction = case _ of
  PropsReceived props -> H.modify_ _ { props = props }
  Changed value -> do
    assign _displayedValue value
    assign _pristine false
    parse <- use (_props <<< _parse)
    H.raise $ ValueChanged $ parse value
  Raise msg -> H.raise $ Emitted msg

handleQuery ::
  forall msg slots m e a n.
  Display a =>
  MonadAff m =>
  Query n ->
  InputM msg slots m e a (Maybe n)
handleQuery = case _ of
  Focus n -> do
    focus inputRef
    pure $ Just n
  Reset n -> do
    modify_ $ initialState <<< _.props
    parse <- use (_props <<< _parse)
    H.raise $ ValueChanged $ parse ""
    pure $ Just n

raise :: forall msg slots m e a. msg -> Action msg slots m e a
raise = Raise
