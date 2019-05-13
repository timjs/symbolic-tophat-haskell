module Language.Inp
  ( Input(..)
  ) where

import Language.Name
import Language.Type


-- Names -----------------------------------------------------------------------


data Input where
  Change :: Name a -> Input
  Continue :: Input

  ToFirst :: Input -> Input
  ToSecond :: Input -> Input
