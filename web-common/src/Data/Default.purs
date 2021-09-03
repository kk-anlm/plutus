module Data.Default where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

class Default a where
  default :: a

instance defaultMaybe :: Default (Maybe a) where
  default = Nothing

instance defaultEither :: Default e => Default (Either e a) where
  default = Left default

instance defaultArray :: Default a => Default (Array a) where
  default = []

instance defaultFunction :: Default a => Default (r -> a) where
  default = const default

instance defaultString :: Default String where
  default = ""

instance defaultInt :: Default Int where
  default = 0

instance defaultBoolean :: Default Boolean where
  default = false
