module Data.Tag.Rec.Internal where

import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (runST)
import Data.Kind (Type)
import qualified Data.Primitive as Primitive
import Data.Tag.Internal
import Data.Tag.Internal.Utils
import GHC.Exts (Any)
import Prelude hiding (lookup, map, mapM_)

type role Rec nominal nominal

newtype Rec :: [Type] -> (Type -> Type) -> Type where
  UnsafeRec :: Primitive.SmallArray Any -> Rec xs f

replicateM :: forall xs f m. (PrimMonad m, Length xs) => (forall x. Tag xs x -> m (f x)) -> m (Rec xs f)
replicateM f = do
  let len = reifyLength @xs
  marr <- Primitive.newSmallArray len undefined
  let go i
        | i < len = do
            res <- f (UnsafeTag i)
            Primitive.writeSmallArray marr i $ unsafeToAny res
            go (i + 1)
        | otherwise = UnsafeRec <$> Primitive.unsafeFreezeSmallArray marr
  go 0

replicate :: forall xs f. Length xs => (forall x. Tag xs x -> f x) -> Rec xs f
replicate f = runST $ replicateM $ pure . f

forM_ :: forall xs f m. Monad m => Rec xs f -> (forall x. Tag xs x -> f x -> m ()) -> m ()
forM_ (UnsafeRec arr) f = do
  let go i
        | i < len = do
            let !(# res #) = Primitive.indexSmallArray## arr i
            f (UnsafeTag i) (unsafeFromAny res)
            go (i + 1)
        | otherwise = pure ()
      len = Primitive.sizeofSmallArray arr
  go 0

map :: forall xs f g. Rec xs f -> (forall x. Tag xs x -> f x -> g x) -> Rec xs g
map vec f = runST $ mapM_ vec $ \tag x -> pure $ f tag x

mapM_ :: forall xs f g m. PrimMonad m => Rec xs f -> (forall x. Tag xs x -> f x -> m (g x)) -> m (Rec xs g)
mapM_ (UnsafeRec arr) f = do
  let len = Primitive.sizeofSmallArray arr
  marr <- Primitive.newSmallArray @_ @Any len undefined
  let go i
        | i < len = do
            res <- Primitive.indexSmallArrayM arr i
            res' <- f (UnsafeTag i) (unsafeFromAny @_ res)
            Primitive.writeSmallArray marr i (unsafeToAny res')
            go (i + 1)
        | otherwise = UnsafeRec <$> Primitive.unsafeFreezeSmallArray marr
  go 0

lookup :: Tag xs x -> Rec xs f -> f x
lookup (UnsafeTag i) (UnsafeRec arr) = unsafeFromAny @_ $ Primitive.indexSmallArray arr i
{-# INLINE lookup #-}
