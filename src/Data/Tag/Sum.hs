{-# LANGUAGE UndecidableInstances #-}

module Data.Tag.Sum where

import Data.GADT.Compare (GCompare (gcompare), GEq (geq), GOrdering (..))
import Data.Kind (Type)
import Data.Tag.Internal (Has', Tag, has')
import Data.Type.Equality ((:~:) (..))

data Sum :: [Type] -> (Type -> Type) -> Type where
  (:=>) :: !(Tag xs x) -> f x -> Sum xs f

instance (Has' Eq xs f) => Eq (Sum xs f) where
  (t :=> x) == (t' :=> x') = case geq t t' of
    Just Refl -> has' @Eq @f t (x == x')
    Nothing -> False

instance (Has' Eq xs f, Has' Ord xs f) => Ord (Sum xs f) where
  compare (t :=> x) (t' :=> x') = case gcompare t t' of
    GLT -> LT
    GGT -> GT
    GEQ -> has' @Ord @f t (compare x x')
