module Input.Text
  ( Input
  , defaultInput
  , component
  ) where

import Css as Css
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HE
import Halogen.HTML.Properties as HP
import Input.Base as Base
import Prelude

-------------------------------------------------------------------------------
-- Input
-------------------------------------------------------------------------------
-- | Input data for text input components
type Input
  = Base.Input ( placeholder :: Maybe String )

-- | A default text input value
defaultInput :: Input
defaultInput =
  { additionalCss: Base.defaultInput.additionalCss
  , id_: Base.defaultInput.id_
  , readOnly: Base.defaultInput.readOnly
  , value: Base.defaultInput.value
  , error: Base.defaultInput.error
  , placeholder: Nothing
  }

-------------------------------------------------------------------------------
-- Component
-------------------------------------------------------------------------------
-- | Renders a text input field
component :: forall q m. Monad m => H.Component HH.HTML q Input Base.Output m
component =
  H.mkComponent
    { initialState: Base.initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = Base.handleAction }
    }
  where
  render state =
    HH.div_
      [ HE.input $ baseInputProps <> inputPropsExtension
      , Base.renderError state
      ]
    where
    baseInputProps = Base.inputProps InputText Css.input state

    inputPropsExtension =
      catMaybes
        [ HP.placeholder <$> state.input.placeholder
        , Just $ HP.autocomplete false
        ]
