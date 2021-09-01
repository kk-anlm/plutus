module Input.Simple
  ( Props
  , defaultProps
  , render
  , render_
  ) where

import Prelude
import Css as Css
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), isNothing)
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Input.Base as IB
import Prim.Row (class Cons)

type Props
  = { additionalCss :: Array String
    , id_ :: Maybe String
    , readOnly :: Boolean
    , value :: String
    , error :: Maybe String
    , placeholder :: Maybe String
    }

defaultProps :: Props
defaultProps =
  { additionalCss: []
  , id_: Nothing
  , readOnly: false
  , value: ""
  , error: Nothing
  , placeholder: Nothing
  }

render ::
  forall action msg slots m label slot _1.
  Cons label (IB.Slot msg slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  SProxy label ->
  slot ->
  Props ->
  (IB.Message msg -> Maybe action) ->
  H.ComponentHTML action slots m
render label slot props@{ additionalCss, id_, readOnly, placeholder } =
  HH.slot label slot IB.component
    { value: props.value
    , error: props.error
    , render: renderInner
    }
  where
  renderInner { value, error } =
    HH.div_
      [ HH.input $ IB.mkInputProps value
          $ catMaybes
              [ pure $ HP.type_ InputText
              , pure $ classNames $ (Css.input $ isNothing error) <> additionalCss
              , HP.id_ <$> id_
              , pure $ HP.readOnly readOnly
              , HP.placeholder <$> placeholder
              ]
      , IB.renderError error
      ]

render_ ::
  forall action msg slots m label slot _1.
  Cons label (IB.Slot msg slot) _1 slots =>
  IsSymbol label =>
  Ord slot =>
  MonadAff m =>
  SProxy label ->
  slot ->
  Props ->
  H.ComponentHTML action slots m
render_ label slot props = render label slot props $ const Nothing
