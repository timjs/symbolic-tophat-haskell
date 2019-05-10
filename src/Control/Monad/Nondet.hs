module Control.Monad.Nondet where

import Control.Monad.List
import Control.Monad.Supply


class Monad m => MonadNondet m where
  merge :: a -> a -> m a

instance Monad m => MonadNondet (ListT m) where
  merge x y = ListT $ pure [ x, y ]

instance MonadNondet m => MonadNondet (StateT s m) where
  merge x y = lift $ merge x y

instance MonadNondet m => MonadNondet (SupplyT s m) where
  merge x y = lift $ merge x y
