module Data.Tag.Vec.Internal where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Data.Kind (Type)
import qualified Data.Primitive as Primitive
import Data.Tag.Internal
import Data.Tag.Internal.Utils
import GHC.Exts (Any)
import Prelude hiding (lookup, map, mapM_)

type role Vec nominal nominal

newtype Vec :: [Type] -> (Type -> Type) -> Type where
  UnsafeVec :: Primitive.SmallArray Any -> Vec xs f

replicateM :: forall xs f m. (PrimMonad m, Length xs) => (forall x. Tag xs x -> m (f x)) -> m (Vec xs f)
replicateM f = do
  let len = reifyLength @xs
  marr <- Primitive.newSmallArray len undefined
  let go i
        | i < len = do
            res <- f (UnsafeTag i)
            Primitive.writeSmallArray marr i $ unsafeToAny res
            go (i + 1)
        | otherwise = UnsafeVec <$> Primitive.unsafeFreezeSmallArray marr
  go 0

forM_ :: forall xs f m. Monad m => Vec xs f -> (forall x. Tag xs x -> f x -> m ()) -> m ()
forM_ (UnsafeVec arr) f = do
  let go i
        | i < len = do
            let !(# res #) = Primitive.indexSmallArray## arr i
            f (UnsafeTag i) (unsafeFromAny res)
            go (i + 1)
        | otherwise = pure ()
      len = Primitive.sizeofSmallArray arr
  go 0

map :: forall xs f g. Vec xs f -> (forall x. Tag xs x -> f x -> g x) -> Vec xs g
map (UnsafeVec arr) f = runST $ do
  let len = Primitive.sizeofSmallArray arr
  marr <- Primitive.newSmallArray @_ @Any len undefined
  let go i
        | i < len = do
            res <- Primitive.indexSmallArrayM arr i
            let res' = f (UnsafeTag i) (unsafeFromAny @_ res)
            Primitive.writeSmallArray marr i (unsafeToAny res')
            go (i + 1)
        | otherwise = UnsafeVec <$> Primitive.unsafeFreezeSmallArray marr
  go 0

mapM_ :: forall xs f g m. PrimMonad m => Vec xs f -> (forall x. Tag xs x -> f x -> m (g x)) -> m (Vec xs g)
mapM_ (UnsafeVec arr) f = do
  let len = Primitive.sizeofSmallArray arr
  marr <- Primitive.newSmallArray @_ @Any len undefined
  let go i
        | i < len = do
            res <- Primitive.indexSmallArrayM arr i
            res' <- f (UnsafeTag i) (unsafeFromAny @_ res)
            Primitive.writeSmallArray marr i (unsafeToAny res')
            go (i + 1)
        | otherwise = UnsafeVec <$> Primitive.unsafeFreezeSmallArray marr
  go 0

lookup :: Tag xs x -> Vec xs f -> f x
lookup (UnsafeTag i) (UnsafeVec arr) = unsafeFromAny @_ $ Primitive.indexSmallArray arr i
{-# INLINE lookup #-}
