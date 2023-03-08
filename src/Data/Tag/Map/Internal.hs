module Data.Tag.Map.Internal where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Kind (Type)
import Data.Tag.Internal (Tag (..))
import Data.Tag.Internal.Utils
import GHC.Exts (Any)

type role Map nominal nominal

newtype Map :: (Type -> Type) -> [Type] -> Type where
  UnsafeMap :: IntMap Any -> Map f xs

empty :: Map f xs
empty = UnsafeMap IM.empty

insert :: Tag xs x -> f x -> Map f xs -> Map f xs
insert (UnsafeTag n) x (UnsafeMap m) = UnsafeMap (IM.insert n (unsafeToAny x) m)

delete :: Tag xs x -> Map f xs -> Map f xs
delete (UnsafeTag n) (UnsafeMap m) = UnsafeMap (IM.delete n m)

lookup :: Tag xs x -> Map f xs -> Maybe (f x)
lookup (UnsafeTag n) (UnsafeMap m) = case IM.lookup n m of
  Nothing -> Nothing
  Just x -> Just (unsafeFromAny x)
