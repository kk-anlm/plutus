module Input.Text
  ( Props
  , defaultProps
  , render
  , module Base
  ) where

import Prelude
import Css as Css
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Input.Base
  ( Action
  , Component
  , Message(..)
  , Query(..)
  , Slot
  , Slots
  , State
  , label
  , defaultHandleMessage
  )
  as Base
import Input.Base (component, mkInputProps, renderError)

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
  forall action msg slots m slot.
  Ord slot =>
  MonadAff m =>
  slot ->
  Props ->
  (Base.Message msg -> action) ->
  H.ComponentHTML action (Base.Slots slots slot msg) m
render slot props@{ additionalCss, id_, readOnly, placeholder } handle =
  HH.slot Base.label slot component
    { value: props.value
    , error: props.error
    , render: renderInner
    }
    (Just <<< handle)
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
