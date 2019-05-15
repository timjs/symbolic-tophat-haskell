module Language.Name
  ( Name(..)
  , fresh
  ) where

import Control.Monad.Supply(MonadSupply(..))
-- import Data.Stream (Stream(..))
import Language.Type (Ty)


-- Names -----------------------------------------------------------------------

newtype Name (a :: Ty)
  = Name Int
  deriving ( Pretty, Eq, Ord, Num ) via Int

-- fresh :: Stream Int -> ( Name t, Stream Int )
-- fresh (Cons i is) = ( Name i, is )

fresh :: MonadSupply Int m => m (Name t)
fresh = do
  i <- supply
  pure $ Name i
