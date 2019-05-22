{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Store
  ( MonadStore(..)
  , StoreT(..), Store
  , runStoreT, evalStoreT
  , runStore, evalStore
  ) where

import Control.Monad.List
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Supply
import Control.Monad.Steps
import Data.Some
import Language.Name
import Language.Type
import Language.Val


class Monad m => MonadStore m where
  new   :: Typeable p => Val ('TyPrim p) -> m (Val ('TyRef p))
  read  :: Typeable p => Val ('TyRef p) -> m (Val ('TyPrim p))
  write :: Typeable p => Val ('TyRef p) -> Val ('TyPrim p) -> m ()

  inspect :: m (List (Some Val))
  place :: List (Some Val) -> m ()

-- | Store monad transformer.
newtype StoreT m p = StoreT (StateT ( Nat, List (Some Val) ) m p)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )

-- | Store monad.
type Store = StoreT Identity

instance Monad m => MonadStore (StoreT m) where
  new x = StoreT do
    ( n, xs ) <- get
    put $ ( succ n, pack x : xs )
    pure $ Loc (Name n)

  read (Loc (Name i)) = StoreT do
    ( _, xs ) <- get
    case indexBack i xs of
      Nothing -> error $ "Language.Store.read: could not find location " <> show i <> " in store " <> show (pretty $ reverse xs)
      Just p -> case unpack p of
        Nothing -> error $ "Language.Store.read: could not unpack location " <> show i <> " from store " <> show (pretty $ reverse xs)
        Just x -> pure x

  write (Loc (Name i)) x = StoreT do
    ( n, xs ) <- get
    put $ ( n, updateBack i (pack x) xs )
    pure ()

  inspect = StoreT do
    ( _, xs ) <- get
    pure xs

  place xs = StoreT do
    put ( nat $ length xs, xs )


instance MonadStore m => MonadStore (SupplyT s m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance MonadStore m => MonadStore (ExceptT e m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance MonadStore m => MonadStore (StateT s m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance MonadStore m => MonadStore (ReaderT r m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance ( Monoid w, MonadStore m ) => MonadStore (Lazy.WriterT w m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance ( Monoid w, MonadStore m ) => MonadStore (Strict.WriterT w m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance ( Monoid w, MonadStore m ) => MonadStore (StepsT w m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

instance MonadStore m => MonadStore (ListT m) where
  new = lift << new
  read = lift << read
  write l = lift << write l
  inspect = lift inspect
  place = lift << place

runStoreT :: Monad m => StoreT m a -> m ( a, List (Some Val) )
runStoreT (StoreT s) = map (\(a, (_, ss)) -> (a, ss)) $ runStateT s ( 0, [] )

evalStoreT :: Monad m => StoreT m a -> m a
evalStoreT (StoreT s) = evalStateT s ( 0, [] )

runStore :: Store a -> ( a, List (Some Val) )
runStore = runIdentity << runStoreT

evalStore :: Store a -> a
evalStore = runIdentity << evalStoreT

--

indexBack :: Nat -> List a -> Maybe a
indexBack n = index n << reverse

index :: Nat -> List a -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index n (_ : xs) = index (pred n) xs

updateBack :: Nat -> a -> List a -> List a
updateBack n y = reverse << update n y << reverse

update :: Nat -> a -> List a -> List a
update _ _ []       = []
update 0 y (_ : xs) = y : xs
update n y (x : xs) = x : update (pred n) y xs
