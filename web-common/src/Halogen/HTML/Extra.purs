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

maybeHTML ::
  forall action slots m a.
  (a -> HH.ComponentHTML action slots m) ->
  Maybe a ->
  HH.ComponentHTML action slots m
maybeHTML = maybe (HH.text "")

maybeHTML' ::
  forall action slots m a.
  Maybe a ->
  (a -> HH.ComponentHTML action slots m) ->
  HH.ComponentHTML action slots m
maybeHTML' = flip maybeHTML

whenHTML ::
  forall action slots m.
  Boolean ->
  HH.ComponentHTML action slots m ->
  HH.ComponentHTML action slots m
whenHTML cond html = case cond of
  true -> html
  false -> HH.text ""
