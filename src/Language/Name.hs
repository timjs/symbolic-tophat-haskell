module Language.Name
  ( Name
  , fresh
  ) where

import Language.Type


-- Names -----------------------------------------------------------------------


newtype Name (a :: Ty)
  = Name Int
  deriving ( Pretty, Eq, Ord, Num ) via Int


fresh :: Name a -> Name b
fresh (Name n) = Name (n + 1)
