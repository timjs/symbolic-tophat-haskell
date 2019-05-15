{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Store
  ( MonadStore(..)
  , StoreT(..), Store(..)
  ) where

import Data.Some
import Language.Name
import Language.Type
import Language.Val


class Monad m => MonadStore m where
  new   :: Typeable a => Val ('TyPrim a) -> m (Val ('TyRef a))
  read  :: Typeable a => Val ('TyRef a) -> m (Val ('TyPrim a))
  write :: Typeable a => Val ('TyRef a) -> Val ('TyPrim a) -> m ()

-- | Store monad transformer.
newtype StoreT m a = StoreT (StateT ( Int, List (Some Val) ) m a)
  deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )

-- | Store monad.
newtype Store a = Store (StateT ( List (Some Val) ) Identity a)
  deriving ( Functor, Applicative, Monad )

instance Monad m => MonadStore (StoreT m) where
  new x = StoreT do
    ( n, xs ) <- get
    put $ ( succ n, pack x : xs )
    pure $ Loc (Name n)

  read (Loc (Name i)) = StoreT do
    ( n, xs ) <- get
    -- let x =  index (n - i) xs >>= unpack >>= fromMaybe (error $ "Language.Store.read: could not find or unpack l" ++ show i)
    case index (n - i) xs >>= unpack of
      Just x -> pure x
      Nothing -> error ""

  write (Loc (Name i)) x = StoreT do
    ( n, xs ) <- get
    put $ ( n, update (n - i) (pack x) xs )
    pure ()


--

index :: Int -> List a -> Maybe a
index _ []       = Nothing
index 0 (x : _)  = Just x
index n (_ : xs) = index (pred n) xs

update :: Int -> a -> List a -> List a
update _ _ []       = []
update 0 y (_ : xs) = y : xs
update n y (x : xs) = x : update (pred n) y xs
