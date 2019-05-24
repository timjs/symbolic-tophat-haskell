module Language.Name
  ( Name(..)
  , fresh
  ) where

import Control.Monad.Supply(MonadSupply(..))
-- import Data.Stream (Stream(..))
import Language.Type (Ty)


-- Names -----------------------------------------------------------------------

newtype Name (a :: Ty)
  = Name Nat
  deriving ( Pretty, Eq, Ord, Num ) via Nat

-- fresh :: Stream Nat -> ( Name t, Stream Nat )
-- fresh (Cons i is) = ( Name i, is )

fresh :: MonadSupply Nat m => m (Name t)
fresh = do
  i <- supply
  pure $ Name i
