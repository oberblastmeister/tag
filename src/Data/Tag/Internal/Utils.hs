{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tag.Internal.Utils where

import Data.Kind (Type)
import GHC.Exts (Any)
import GHC.TypeLits
import qualified Unsafe.Coerce

class Length (xs :: [Type]) where
  reifyLength :: Int
  reifyLength = error "reifyLength"

instance Length '[] where
  reifyLength = 0

instance Length xs => Length (x ': xs) where
  reifyLength = 1 + reifyLength @xs

class (:>) (x :: Type) (xs :: [Type]) where
  reifyIndex :: Int
  reifyIndex = error "reifyIndex"

instance
  TypeError
    ( Text "The type '" :<>: ShowType x :<>: Text "' is not in the list"
    ) =>
  x :> '[]
  where
  reifyIndex = error "unreachable"

instance {-# OVERLAPPING #-} x :> (x : xs) where
  reifyIndex = 0

instance x :> xs => x :> (y : xs) where
  reifyIndex = 1 + reifyIndex @x @xs

unsafeToAny :: a -> Any
unsafeToAny = Unsafe.Coerce.unsafeCoerce

unsafeFromAny :: Any -> a
unsafeFromAny = Unsafe.Coerce.unsafeCoerce
