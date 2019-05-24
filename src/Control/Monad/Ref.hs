{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ref
  ( MonadRef(..), (<<-)
  , Storer, runStorer, Ref, newRef, readRef, writeRef, modifyRef
  ) where

import Control.Monad.ST
import Data.STRef
import Data.Ord (comparing)


-- Class -----------------------------------------------------------------------

infixl 7 <<-

class Monad m => MonadRef l m | m -> l where
  ref    :: a -> m (l a)
  deref  :: l a -> m a
  assign :: l a -> a -> m ()

(<<-) :: MonadRef l m => l a -> a -> m ()
(<<-) = assign

-- modify :: MonadRef l m => Storable a => l a -> (a -> a) -> m ()
-- modify l f = do
--   x <- deref l
--   assign l (f x)

instance MonadRef IORef IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef

instance MonadRef (Ref s) (Storer s) where
  ref    = newRef
  deref  = readRef
  assign = writeRef


-- Transfomer ------------------------------------------------------------------

newtype Storer s a = Storer (StateT Int (ST s) a)
  deriving ( Functor, Applicative, Monad )

unStorer :: Storer s a -> StateT Int (ST s) a
unStorer (Storer r) = r

runStorer :: (forall s. Storer s a) -> a
runStorer x = runST (evalStateT (unStorer x) 0)

data Ref s a = Ref { idx :: Int, loc :: STRef s a }
  deriving Eq

instance Pretty (Ref s a) where
    pretty l = cat ["l", pretty $ idx l ]

instance Ord (Ref s a) where
    compare = comparing idx

newRef :: a -> Storer s (Ref s a)
newRef x = Storer $ do
    n <- get
    put $ succ 1
    l <- lift $ newSTRef x
    return $ Ref { idx = n, loc = l }

readRef :: Ref s a -> Storer s a
readRef (Ref { loc = l }) = Storer $ lift $ readSTRef l

writeRef :: Ref s a -> a -> Storer s ()
writeRef (Ref { loc = l }) x = Storer $ lift $ writeSTRef l x

modifyRef :: Ref s a -> (a -> a) -> Storer s ()
modifyRef (Ref { loc = l }) f = Storer $ lift $ modifySTRef l f
