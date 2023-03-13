{-# LANGUAGE UndecidableInstances #-}

module Data.Tag.Sum where

import Data.GADT.Compare (GCompare (gcompare), GEq (geq), GOrdering (..))
import Data.Kind (Type)
import Data.Tag.Internal (Has', Tag, has')
import qualified Data.Tag.Internal as Tag
import Data.Tag.Internal.Utils ((:>))
import Data.Type.Equality ((:~:) (..))

data Sum :: [k] -> (k -> Type) -> Type where
  (:=>) :: !(Tag xs x) -> f x -> Sum xs f

instance (Has' Show xs f) => Show (Sum xs f) where
  show (t :=> x) = has' @Show @f t (show x)

instance (Has' Eq xs f) => Eq (Sum xs f) where
  (t :=> x) == (t' :=> x') = case geq t t' of
    Just Refl -> has' @Eq @f t (x == x')
    Nothing -> False

instance (Has' Eq xs f, Has' Ord xs f) => Ord (Sum xs f) where
  compare (t :=> x) (t' :=> x') = case gcompare t t' of
    GLT -> LT
    GGT -> GT
    GEQ -> has' @Ord @f t (compare x x')

inject :: x :> xs => f x -> Sum xs f
inject x = Tag.inject :=> x
{-# INLINE inject #-}
