module Language.Names
  ( Name(..)
  ) where

import Language.Types


-- Names -----------------------------------------------------------------------


newtype Name (a :: Ty)
  = Name Int
  deriving ( Pretty, Eq, Ord, Num ) via Int
