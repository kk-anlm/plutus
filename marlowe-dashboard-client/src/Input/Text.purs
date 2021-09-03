module Input.Text
  ( Props
  , defaultProps
  , render
  , render_
  , module Base
  ) where

import Prelude
import Css as Css
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (catMaybes)
import Data.Default (default)
import Data.Display (class Display)
import Data.Either (Either)
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Input.Base (Action, Component, Message(..), Query(..), Slot, State, defaultHandleMessage) as Base
import Input.Base (component, mkInputProps, renderError)
import Prim.Row (class Cons)

type Props e a
  = { additionalCss :: Array String
    , id_ :: Maybe String
    , placeholder :: Maybe String
    , readOnly :: Boolean
    , value :: Either e a
    , parse :: String -> Either e a
    }

defaultProps :: forall e a. Either e a -> (String -> Either e a) -> Props e a
defaultProps value parse =
  { additionalCss: default
  , id_: default
  , readOnly: default
  , value
  , placeholder: default
  , parse
  }

renderImpl ::
  forall action msg slots m e a slot label _1.
  Cons label (Base.Slot msg e a slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  Display e =>
  Display a =>
  SProxy label ->
  slot ->
  Props e a ->
  (Base.Message msg e a -> Maybe action) ->
  H.ComponentHTML action slots m
renderImpl label slot props@{ additionalCss, id_, readOnly, placeholder } =
  HH.slot label slot component
    { value: props.value
    , parse: props.parse
    , render: renderInner
    }
  where
  renderInner { value, error } =
    HH.div_
      [ HH.input $ mkInputProps value
          $ catMaybes
              [ pure $ HP.type_ InputText
              , pure $ classNames $ (Css.input $ isNothing error) <> additionalCss
              , HP.id_ <$> id_
              , pure $ HP.readOnly readOnly
              , HP.placeholder <$> placeholder
              ]
      , renderError error
      ]

render ::
  forall action msg slots m e a slot label _1.
  Cons label (Base.Slot msg e a slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  Display e =>
  Display a =>
  SProxy label ->
  slot ->
  Props e a ->
  (Base.Message msg e a -> action) ->
  H.ComponentHTML action slots m
render label slot props@{ additionalCss, id_, readOnly, placeholder } handle = renderImpl label slot props $ Just <<< handle

render_ ::
  forall action msg slots m e a slot label _1.
  Cons label (Base.Slot msg e a slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  Display e =>
  Display a =>
  SProxy label ->
  slot ->
  Props e a ->
  H.ComponentHTML action slots m
render_ label slot props@{ additionalCss, id_, readOnly, placeholder } = renderImpl label slot props $ const Nothing
