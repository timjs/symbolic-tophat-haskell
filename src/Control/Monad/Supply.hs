-- | Support for computations which consume values from an _infinite_ supply.
-- | See <http://www.haskell.org/haskellwiki/New_monads/MonadSupply> for more details.
-- FIXME: Make use of DerivingVia
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Supply
  ( MonadSupply (..)
  , SupplyT(..), Supply
  , evalSupplyT, evalSupply
  , runSupplyT, runSupply
  -- , supplies
  ) where

import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Root

import Data.Stream (Stream(..))
import qualified Data.Stream as Stream

-- | MonadSupply class to support getting the next item from a stream
-- | and peaking into the next item.
class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  peek :: m s

-- | Supply monad transformer.
newtype SupplyT s m a = SupplyT (StateT (Stream s) m a)
  deriving ( Functor, Applicative, Monad, MonadWriter w, MonadReader r, MonadTrans, MonadIO, MonadFix )

-- | Supply monad.
type Supply s = SupplyT s Identity

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT do
    (Cons x xs) <- get
    put xs
    pure x
  peek = SupplyT $ gets Stream.head

instance Semigroup a => Semigroup (Supply s a) where
  m1 <> m2 = pure (<>) <*> m1 <*> m2

instance (Semigroup a, Monoid a) => Monoid (Supply s a) where
  mempty = pure mempty


-- Monad transformer instances --

instance MonadSupply s m => MonadSupply s (ExceptT e m) where
  supply = lift supply
  peek = lift peek

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  peek = lift peek

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  peek = lift peek

instance ( Monoid w, MonadSupply s m ) => MonadSupply s (Lazy.WriterT w m) where
  supply = lift supply
  peek = lift peek

instance ( Monoid w, MonadSupply s m ) => MonadSupply s (Strict.WriterT w m) where
  supply = lift supply
  peek = lift peek

instance MonadSupply s m => MonadSupply s (ListT m) where
  supply = lift supply
  peek = lift peek

instance ( Monoid v, MonadSupply s m ) => MonadSupply s (RootT v m) where
  supply = lift supply
  peek = lift peek


-- Running and evaluating --

-- -- | Get n supplies.
-- supplies :: MonadSupply s m => Int -> m (Stream s)
-- supplies n = replicateM n supply

runSupplyT :: SupplyT s m a -> Stream s -> m ( a, Stream s )
runSupplyT (SupplyT s) = runStateT s

evalSupplyT :: Monad m => SupplyT s m a -> Stream s -> m a
evalSupplyT (SupplyT s) = evalStateT s

runSupply :: Supply s a -> Stream s -> ( a, Stream s )
runSupply s = runIdentity << runSupplyT s

evalSupply :: Supply s a -> (Stream s) -> a
evalSupply s = runIdentity << evalSupplyT s
