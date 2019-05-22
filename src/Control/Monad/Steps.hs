module Control.Monad.Steps where

import Data.Steps (Steps)


newtype StepsT m a = StepsT (m (Steps a))
  deriving ( Functor, Foldable, Traversable )


runStepsT :: StepsT m a -> m (Steps a)
runStepsT (StepsT s) = s


instance Applicative m => Applicative (StepsT m) where
  pure x = StepsT $ pure (pure x)

  f <*> x = StepsT $ pure (<*>) <*> runStepsT f <*> runStepsT x


instance Applicative m => Alternative (StepsT m) where
  empty = StepsT $ pure empty

  ls <|> rs = StepsT $ pure (<|>) <*> runStepsT ls <*> runStepsT rs


instance Monad m => Monad (StepsT m) where
  m >>= k = StepsT do
    x <- runStepsT m
    y <- traverse (runStepsT << k) x
    pure $ join y


instance Monad m => MonadFail (StepsT m) where
  fail _ = StepsT $ pure empty

instance Monad m => MonadZero (StepsT m)
instance Monad m => MonadPlus (StepsT m)


instance MonadTrans StepsT where
  lift ma = StepsT do
    a <- ma
    pure $ pure a
