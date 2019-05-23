module Control.Monad.Steps where

import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Steps (Steps)

import Data.Steps as Steps


newtype StepsT h m a = StepsT (m (Steps h a))
  deriving ( Functor, Foldable, Traversable )


runStepsT :: StepsT h m a -> m (Steps h a)
runStepsT (StepsT s) = s


instance ( Monoid h, Applicative m ) => Applicative (StepsT h m) where
  pure x = StepsT $ pure (pure x)

  f <*> x = StepsT $ pure (<*>) <*> runStepsT f <*> runStepsT x


instance ( Monoid h, Applicative m ) => Alternative (StepsT h m) where
  empty = StepsT $ pure empty

  ls <|> rs = StepsT $ pure (<|>) <*> runStepsT ls <*> runStepsT rs


instance ( Monoid h, Monad m ) => Monad (StepsT h m) where
  m >>= k = StepsT do
    x <- runStepsT m
    y <- traverse (runStepsT << k) x
    pure $ join y


instance ( Monoid h, Monad m ) => MonadFail (StepsT h m) where
  fail _ = StepsT $ pure empty

instance ( Monoid h, Monad m ) => MonadZero (StepsT h m)
instance ( Monoid h, Monad m ) => MonadPlus (StepsT h m)


instance Monoid h => MonadTrans (StepsT h) where
  lift ma = StepsT do
    a <- ma
    pure $ pure a


instance ( Monoid w, Monad m ) => MonadWriter w (StepsT w m) where
  tell = StepsT << pure << None

  listen m = StepsT do
    xs <- runStepsT m
    pure $ Steps.listen xs

  pass m = StepsT do
    xs <- runStepsT m
    pure $ Steps.pass xs
