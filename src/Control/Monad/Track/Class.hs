module Control.Monad.Track.Class
  ( MonadTrack(..)
  ) where

import Control.Monad.Root

import Data.Root (save)


class ( Monoid v, Monad m ) => MonadTrack v m | m -> v where
  track :: v -> m a -> m a


-- instance MonadTrack v m => MonadTrack v (ExceptT e m) where
--   track v m = lift _

-- instance MonadTrack v m => MonadTrack v (Lazy.StateT st m) where
--   track v = lift << track v

instance MonadTrack v m => MonadTrack v (StateT st m) where
  track v m = StateT \s -> do
    ~( a, s' ) <- track v (runStateT m s)
    pure ( a, s' )

-- instance MonadTrack v m => MonadTrack v (ReaderT r m) where
--   track v = lift << track v

-- instance ( Monoid w, MonadTrack v m ) => MonadTrack v (Lazy.WriterT w m) where
--   track v = lift << track v

-- instance ( Monoid w, MonadTrack v m ) => MonadTrack v (Strict.WriterT w m) where
--   track v = lift << track v

-- instance MonadTrack v m => MonadTrack v (ListT m) where
--   track v = lift << track v

instance ( Monoid v, Monad m ) => MonadTrack v (RootT v m) where
  track v m = RootT do
    xs <- runRootT m
    pure $ save v xs
