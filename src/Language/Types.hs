module Language.Types
  ( module Data.Universe
  , Ty(..)
  ) where


import Preload

import Data.Universe



-- Universe --------------------------------------------------------------------


data {- kind -} Ty
  = Ty :-> Ty
  | Ty :>< Ty

  | TyUnit
  | TyBool
  | TyInt
  | TyString


instance Universe Ty where
  type TypeOf (a ':-> b) = TypeOf a -> TypeOf b
  type TypeOf (a ':>< b) = ( TypeOf a, TypeOf b )

  type TypeOf 'TyUnit = ()
  type TypeOf 'TyBool = Bool
  type TypeOf 'TyInt = Int
  type TypeOf 'TyString = Text



-- Basics ----------------------------------------------------------------------


class (Pretty (TypeOf t)) => IsBasic t


instance IsBasic 'TyUnit
instance IsBasic 'TyBool
instance IsBasic 'TyInt
instance IsBasic 'TyString
instance ( IsBasic a, IsBasic b ) => IsBasic (a ':>< b)
