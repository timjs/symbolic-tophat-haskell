-- | Support for computations which consume values from an _infinite_ supply.
-- | See <http://www.haskell.org/haskellwiki/New_monads/MonadSupply> for details.
-- FIXME: Make use of DerivingVia
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Supply
  ( MonadSupply (..)
  , SupplyT(..), Supply
  , evalSupplyT, evalSupply
  , runSupplyT, runSupply
  -- , supplies
  ) where

import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Writer

import Data.Stream (Stream(..))
import qualified Data.Stream as Stream


class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  peek :: m s

-- | Supply monad transformer.
newtype SupplyT s m a = SupplyT (StateT (Stream s) m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix )

-- | Supply monad.
newtype Supply s a = Supply (SupplyT s Identity a)
  deriving ( Functor, Applicative, Monad, MonadSupply s, MonadFix )

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT do
    (Cons x xs) <- get
    put xs
    pure x
  peek = SupplyT $ gets Stream.head

-- Monad transformer instances
instance (MonadSupply s m) => MonadSupply s (ExceptT e m) where
  supply = lift supply
  peek = lift peek

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  peek = lift peek

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  peek = lift peek

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
  peek = lift peek

instance MonadSupply s m => MonadSupply s (ListT m) where
  supply = lift supply
  peek = lift peek

instance Semigroup a => Semigroup (Supply s a) where
  m1 <> m2 = pure (<>) <*> m1 <*> m2

instance (Semigroup a, Monoid a) => Monoid (Supply s a) where
  mempty = pure mempty

-- -- | Get n supplies.
-- supplies :: MonadSupply s m => Int -> m (Stream s)
-- supplies n = replicateM n supply

evalSupplyT :: Monad m => SupplyT s m a -> Stream s -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> (Stream s) -> a
evalSupply (Supply s) = runIdentity << evalSupplyT s

runSupplyT :: SupplyT s m a -> Stream s -> m ( a, Stream s )
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> Stream s -> ( a, Stream s )
runSupply (Supply s) = runIdentity << runSupplyT s
