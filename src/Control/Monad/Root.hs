module Control.Monad.Root
  ( RootT(..), runRootT
  ) where

import Data.Root (Root)


newtype RootT v m a = RootT (m (Root v a))
  deriving ( Functor, Foldable, Traversable )


runRootT :: RootT v m a -> m (Root v a)
runRootT (RootT xs) = xs


instance ( Monoid v, Applicative m ) => Applicative (RootT v m) where
  pure x = RootT $ pure (pure x)

  f <*> x = RootT $ pure (<*>) <*> runRootT f <*> runRootT x


instance ( Monoid v, Applicative m ) => Alternative (RootT v m) where
  empty = RootT $ pure empty

  ls <|> rs = RootT $ pure (<|>) <*> runRootT ls <*> runRootT rs


instance ( Monoid v, Monad m ) => Monad (RootT v m) where
  m >>= k = RootT do
    x <- runRootT m
    y <- traverse (runRootT << k) x
    pure $ join y


instance ( Monoid v, Monad m ) => MonadFail (RootT v m) where
  fail _ = RootT $ pure empty

instance ( Monoid v, Monad m ) => MonadZero (RootT v m)
instance ( Monoid v, Monad m ) => MonadPlus (RootT v m)


instance ( Monoid v ) => MonadTrans (RootT v) where
  lift ma = RootT do
    a <- ma
    pure $ pure a


{-
instance ( Monoid w, Monad m ) => MonadWriter w (RootT w m) where
  tell = RootT << pure << None

  listen m = RootT do
    xs <- runRootT m
    pure $ Root.listen xs

  pass m = RootT do
    xs <- runRootT m
    pure $ Root.pass xs


listen :: Root w a -> Root w ( a, w )
listen = \case
  None w -> None w
  End w x -> End w ( x, w )
  Mid w ls rs -> Mid w (listen ls) (listen rs)


pass :: Root w ( a, w -> w ) -> Root w a
pass = \case
  None w -> None w
  End w ( x, f ) -> End (f w) x
  Mid w ls rs -> Mid w (pass ls) (pass rs)
-}
