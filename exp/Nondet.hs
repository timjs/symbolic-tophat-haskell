{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Nondet where

import Control.Monad
import Control.Monad.Trans


class Monad m => MonadNondet m


-- | Nondet monad transformer.
newtype NondetT m a = NondetT (forall b. (a -> m b -> m b) -> m b -> m b)

runNondetT :: Monad m => NondetT m a -> m a
runNondetT m = foldNondetT m (\x xs -> pure x) (error "No solution found.")

instance Functor m => Functor (NondetT m) where
  fmap f (NondetT g) = NondetT (\cons nil -> g (cons << f) nil)

instance Applicative m => Applicative (NondetT m) where
  pure a = NondetT (\cons nil -> cons a nil)

instance Alternative m => Alternative (NondetT m) where

instance Monad m => Monad (NondetT m) where
  m >>= k  = NondetT (\cons nil -> foldNondetT m (\x -> foldNondetT (k x) cons) nil)

instance Monad m => MonadPlus (NondetT m) where
  mzero         = NondetT (\cons nil -> nil)
  m1 `mplus` m2 = NondetT (\cons -> foldNondetT m1 cons << foldNondetT m2 cons)

instance MonadTrans NondetT where
  lift m = NondetT (\cons nil -> m >>= \a -> cons a nil)

newtype Nondet a = Nondet (NondetT Identity a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runNondet (Nondet x) = runIdentity (runNondetT x)

foldNOndetT (Nondet nd) = nd

foldNondet :: Nondet a -> (a -> b -> b) -> b -> b
foldNondet (Nondet nd) cons nil =
   runIdentity $ foldNondetT nd (\x xs -> pure (cons x (runIdentity xs))) (pure nil)

-- option :: (MonadPlus m) => [a] -> m a
-- option = msum << map pure
