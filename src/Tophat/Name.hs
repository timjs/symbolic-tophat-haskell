module Tophat.Name
  ( Name(..), unname
  , fresh
  ) where

import Control.Monad.Supply(MonadSupply(..))
import Tophat.Type (Ty)


-- Names -----------------------------------------------------------------------

newtype Name (a :: Ty)
  = Name Nat
  deriving ( Pretty, Eq, Ord, Num, Hashable ) via Nat

unname :: Name a -> Nat
unname (Name n) = n

fresh :: MonadSupply Nat m => m (Name t)
fresh = do
  i <- supply
  pure $ Name i
