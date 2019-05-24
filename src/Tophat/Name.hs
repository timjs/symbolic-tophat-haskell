module Tophat.Name
  ( Name(..)
  , fresh
  ) where

import Control.Monad.Supply(MonadSupply(..))
import Tophat.Type (Ty)


-- Names -----------------------------------------------------------------------

newtype Name (a :: Ty)
  = Name Nat
  deriving ( Pretty, Eq, Ord, Num ) via Nat

fresh :: MonadSupply Nat m => m (Name t)
fresh = do
  i <- supply
  pure $ Name i
