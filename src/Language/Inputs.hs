module Language.Inputs
  ( Input(..)
  ) where

import Language.Names
import Language.Types


-- Names -----------------------------------------------------------------------


data Input (a :: Ty) where
  SChange :: Name a -> Input a
  ToFirst :: Input a -> Input a
  ToSecond :: Input a -> Input a
