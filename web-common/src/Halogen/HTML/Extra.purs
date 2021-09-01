module Halogen.HTML.Extra (mapComponent, maybeHTML, maybeHTML', whenHTML) where

import Prelude
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH

mapComponent ::
  forall m a b slots.
  (a -> b) ->
  HH.ComponentHTML a slots m ->
  HH.ComponentHTML b slots m
mapComponent f = bimap (map f) f

maybeHTML :: forall p i a. (a -> HH.HTML p i) -> Maybe a -> HH.HTML p i
maybeHTML = maybe (HH.text "")

maybeHTML' :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeHTML' = flip maybeHTML

whenHTML :: forall p i. Boolean -> HH.HTML p i -> HH.HTML p i
whenHTML cond html = case cond of
  true -> html
  false -> HH.text ""
