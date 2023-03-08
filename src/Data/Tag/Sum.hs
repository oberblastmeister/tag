module Data.Tag.Sum where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Tag.Internal (Tag)

data Sum :: (Type -> Type) -> [Type] -> Type where
  (:=>) :: !(Tag xs x) -> f x -> Sum f xs
