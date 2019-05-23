module Control.Monad.Steps where

import Data.Steps


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


{-
instance ( Monoid w, Monad m ) => MonadWriter w (StepsT w m) where
  tell = StepsT << pure << None

  listen m = StepsT do
    xs <- runStepsT m
    pure $ Steps.listen xs

  pass m = StepsT do
    xs <- runStepsT m
    pure $ Steps.pass xs


listen :: Steps w a -> Steps w ( a, w )
listen = \case
  None w -> None w
  End w x -> End w ( x, w )
  Mid w ls rs -> Mid w (listen ls) (listen rs)


pass :: Steps w ( a, w -> w ) -> Steps w a
pass = \case
  None w -> None w
  End w ( x, f ) -> End (f w) x
  Mid w ls rs -> Mid w (pass ls) (pass rs)
-}
