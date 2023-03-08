{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Tag.Internal.Utils where

import Data.Kind (Constraint)
import GHC.Exts (Any)
import GHC.TypeLits
import qualified Unsafe.Coerce

class Length xs where
  reifyLength :: Int
  reifyLength = error "reifyLength"

instance Length '[] where
  reifyLength = 0

instance Length xs => Length (x ': xs) where
  reifyLength = 1 + reifyLength @xs

class (:>) x xs where
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

-- | Composition for constraints.
class p (f a) => ComposeC (p :: k2 -> Constraint) (f :: k1 -> k2) (a :: k1)

instance p (f a) => ComposeC p f a
