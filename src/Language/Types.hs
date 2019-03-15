module Language.Types
  ( module Data.Universe
  , Ty(..), PrimTy(..), IsBasic
  ) where


import Data.SBV ( HasKind )

import Data.Universe



-- Universe --------------------------------------------------------------------


data {- kind -} Ty
  = Ty :-> Ty
  | Ty :>< Ty
  | TyUnit
  | TyPrim PrimTy


data {- kind -} PrimTy
  = TyBool
  | TyInt
  | TyString


instance Universe Ty where
  type TypeOf (a ':-> b) = TypeOf a -> TypeOf b
  type TypeOf (a ':>< b) = ( TypeOf a, TypeOf b )
  type TypeOf 'TyUnit = ()
  type TypeOf ('TyPrim p) = TypeOf p


instance Universe PrimTy where
  type TypeOf 'TyBool = Bool
  type TypeOf 'TyInt = Integer
  type TypeOf 'TyString = String



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
