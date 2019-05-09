module Language.Inp
  ( Input(..)
  ) where

import Language.Name
import Language.Type


-- Names -----------------------------------------------------------------------


data Input (a :: Ty) where
  SChange :: Name a -> Input a
  ToFirst :: Input a -> Input a
  ToSecond :: Input a -> Input a
