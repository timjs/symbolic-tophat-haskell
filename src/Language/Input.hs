module Language.Input
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

instance Pretty Input where
  pretty = \case
    Change n -> cat [ "s", pretty n ]
    Continue -> "C"
    
    ToFirst i -> sep [ "F", pretty i ]
    ToSecond i -> sep [ "S", pretty i ]
