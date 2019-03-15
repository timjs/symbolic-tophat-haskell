module Language.Types
  ( module Data.Universe
  , Ty(..), IsBasic
  ) where


import Data.SBV

import Data.Universe



-- Universe --------------------------------------------------------------------


data {- kind -} Ty
  = Ty :-> Ty
  | Ty :>< Ty

  | TyUnit
  | TyBool
  | TyInt
  | TyString



-- Basics ----------------------------------------------------------------------


type IsBasic a = (Typeable a, HasKind a, Pretty a)

-- class IsBasic (a :: Ty)
--
-- instance IsBasic 'TyUnit
-- instance IsBasic 'TyBool
-- instance IsBasic 'TyInt
-- instance IsBasic 'TyString
--
-- instance ( IsBasic a, IsBasic b ) => IsBasic (a ':>< b)
