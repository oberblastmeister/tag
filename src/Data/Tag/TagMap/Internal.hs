module Data.Tag.TagMap.Internal where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Kind (Type)
import Data.Tag.Internal (Tag (..))
import Data.Tag.Internal.Utils
import Data.Tag.Sum (Sum (..))
import GHC.Exts (Any)

type role Map nominal nominal

newtype Map :: [Type] -> (Type -> Type) -> Type where
  UnsafeMap :: IntMap Any -> Map f xs

empty :: Map xs f
empty = UnsafeMap IM.empty

insert :: Tag xs x -> f x -> Map xs f -> Map xs f
insert (UnsafeTag n) x (UnsafeMap m) = UnsafeMap (IM.insert n (unsafeToAny x) m)

delete :: Tag xs x -> Map xs f -> Map xs f
delete (UnsafeTag n) (UnsafeMap m) = UnsafeMap (IM.delete n m)

lookup :: Tag xs x -> Map xs f -> Maybe (f x)
lookup (UnsafeTag n) (UnsafeMap m) = case IM.lookup n m of
  Nothing -> Nothing
  Just x -> Just (unsafeFromAny x)

toList :: Map xs f -> [Sum xs f]
toList (UnsafeMap m) = map (\(n, x) -> UnsafeTag n :=> unsafeFromAny x) (IM.toList m)
