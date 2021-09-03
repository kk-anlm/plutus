module Data.Display where

import Data.Show (show)
import Data.Void (Void, absurd)

-- | Display instances are intended to render data for presentation on a UI.
-- | This is an identical class to Show, but with different semantics. Show
-- | instances are intended to produce parsable text, whereas Display instances
-- | are intended to display human-readable text.
class Display a where
  display :: a -> String

instance displayString :: Display String where
  display = show

instance displayInt :: Display Int where
  display = show

instance displayBoolean :: Display Boolean where
  display = show

instance displayVoid :: Display Void where
  display = absurd
