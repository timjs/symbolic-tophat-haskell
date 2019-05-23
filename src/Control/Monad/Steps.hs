module Control.Monad.Steps where

import Data.Steps (Steps)

import Data.Steps as Steps


newtype StepsT t m a = StepsT (m (Steps t a))
  deriving ( Functor, Foldable, Traversable )


runStepsT :: StepsT t m a -> m (Steps t a)
runStepsT (StepsT xs) = xs


instance ( Monoid t, Applicative m ) => Applicative (StepsT t m) where
  pure x = StepsT $ pure (pure x)

  f <*> x = StepsT $ pure (<*>) <*> runStepsT f <*> runStepsT x


instance ( Monoid t, Applicative m ) => Alternative (StepsT t m) where
  empty = StepsT $ pure empty

  ls <|> rs = StepsT $ pure (<|>) <*> runStepsT ls <*> runStepsT rs


instance ( Monoid t, Monad m ) => Monad (StepsT t m) where
  m >>= k = StepsT do
    x <- runStepsT m
    y <- traverse (runStepsT << k) x
    pure $ join y


instance ( Monoid t, Monad m ) => MonadFail (StepsT t m) where
  fail _ = StepsT $ pure empty

instance ( Monoid t, Monad m ) => MonadZero (StepsT t m)
instance ( Monoid t, Monad m ) => MonadPlus (StepsT t m)


instance ( Monoid t ) => MonadTrans (StepsT t) where
  lift ma = StepsT do
    a <- ma
    pure $ pure a


{-
class ( Monoid t, Monad m ) => MonadTrack t m | m -> t where
  track :: t -> m a -> m a

instance ( Monoid t, Monad m ) => MonadTrack t (StepsT t m) where
  track t m = StepsT do
    xs <- runStepsT m
    pure $ Steps.save t xs
-}

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
