{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Tag.Internal where

import Control.DeepSeq (NFData (rnf))
import Data.Function (on)
import Data.GADT.Compare
  ( GCompare (..),
    GEq (..),
    GOrdering (..),
  )
import Data.GADT.DeepSeq (GNFData (grnf))
import Data.GADT.Show (GShow)
import qualified Data.GADT.Show
import Data.Hashable (Hashable (..))
import Data.Kind (Constraint, Type)
import Data.Tag.Internal.Utils
import Type.Reflection ((:~:) (..))
import qualified Unsafe.Coerce

type role Tag nominal nominal

newtype Tag :: [Type] -> Type -> Type where
  UnsafeTag :: {tag :: Int} -> Tag xs x

data STag :: [Type] -> Type -> Type where
  SThis :: STag (x : xs) x
  SThat :: Tag xs x -> STag (y : xs) x

tagSing :: Tag xs x -> STag xs x
tagSing (UnsafeTag 0) = Unsafe.Coerce.unsafeCoerce SThis
tagSing (UnsafeTag n) = Unsafe.Coerce.unsafeCoerce (SThat (UnsafeTag (n - 1)))

pattern This :: () => (x : xs') ~ xs => Tag xs x
pattern This <- (tagSing -> SThis)
  where
    This = UnsafeTag 0

pattern That :: () => (y : xs') ~ xs => Tag xs' x -> Tag xs x
pattern That sum <- (tagSing -> SThat sum)
  where
    That (UnsafeTag n) = UnsafeTag (n + 1)

{-# COMPLETE This, That #-}

inject :: forall x xs. x :> xs => Tag xs x
inject = UnsafeTag $ reifyIndex @x @xs
{-# INLINE inject #-}

project :: forall y x xs. y :> xs => Tag xs x -> Maybe (x :~: y)
project (UnsafeTag tag) =
  if tag == reifyIndex @y @xs
    then Just (Unsafe.Coerce.unsafeCoerce Refl)
    else Nothing
{-# INLINE project #-}

absurd :: Tag '[] x -> a
absurd = undefined

class Has (c :: Type -> Constraint) (xs :: [Type]) where
  has :: Tag xs x -> (c x => r) -> r

instance Has c '[] where
  has u _ = absurd u

instance (c x, Has c xs) => Has c (x ': xs) where
  has This r = r
  has (That t) r = has @c @xs t r

instance Show (Tag xs x) where
  show (UnsafeTag i) = show $ "Tag " ++ show i

instance Eq (Tag xs x) where
  (==) = (==) `on` tag
  {-# INLINE (==) #-}

instance Ord (Tag xs x) where
  compare = compare `on` tag
  {-# INLINE compare #-}

instance Hashable (Tag xs x) where
  hashWithSalt s (UnsafeTag n) = hashWithSalt s n
  {-# INLINE hashWithSalt #-}

instance NFData (Tag xs x) where
  rnf (UnsafeTag n) = rnf n

instance GShow (Tag xs) where
  gshowsPrec = showsPrec

instance GEq (Tag xs) where
  geq (UnsafeTag i) (UnsafeTag i') =
    if i == i'
      then Just $ Unsafe.Coerce.unsafeCoerce Refl
      else Nothing
  {-# INLINE geq #-}

instance GCompare (Tag xs) where
  gcompare (UnsafeTag i) (UnsafeTag i') =
    case compare i i' of
      LT -> GLT
      EQ -> Unsafe.Coerce.unsafeCoerce GEQ
      GT -> GGT
  {-# INLINE gcompare #-}

instance GNFData (Tag xs) where
  grnf = rnf
