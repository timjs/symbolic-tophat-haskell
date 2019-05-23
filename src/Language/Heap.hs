module Language.Heap
  ( Heap
  , Some
  , new, read, write
  ) where

import Language.Val

import Data.Some (Some, pack, unpack)


type Heap = Vector (Some Val)

new   :: MonadState Heap m => Typeable p => Val ('TyPrim p) -> m (Val ('TyRef p))
new x = do
  xs <- get
  put $ xs <> only (pack x)
  pure $ Loc (Name (nat (length xs)))

read  :: MonadState Heap m => Typeable p => Val ('TyRef p) -> m (Val ('TyPrim p))
read (Loc (Name n)) = do
  xs <- get
  case index n xs of
    Nothing -> error $ "Language.Store.read: could not find location " <> show n <> " in store " <> show (pretty xs)
    Just p -> case unpack p of
      Nothing -> error $ "Language.Store.read: could not unpack location " <> show n <> " from store " <> show (pretty xs)
      Just x -> pure x

write :: MonadState Heap m => Typeable p => Val ('TyRef p) -> Val ('TyPrim p) -> m ()
write (Loc (Name n)) x = do
  xs <- get
  put $ update n (pack x) xs
  pure ()
