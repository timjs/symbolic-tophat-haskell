module Tophat.Input
  ( module Tophat.Name
  , Input(..)
  ) where

import Tophat.Name


-- Names -----------------------------------------------------------------------

data Input where
  Change :: Name a -> Input
  Continue :: Input -- FIXME: move to predicates

  ToFirst :: Input -> Input
  ToSecond :: Input -> Input


instance Pretty Input where
  pretty = \case
    Change n -> cat [ "s", pretty n ]
    Continue -> "C"

    ToFirst i -> sep [ "F", pretty i ]
    ToSecond i -> sep [ "S", pretty i ]
