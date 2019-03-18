module Language.Types
  ( module Data.Universe
  , Ty(..), IsPrim(..)
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


instance Universe Ty where
  type TypeOf (a ':-> b) = TypeOf a -> TypeOf b
  type TypeOf (a ':>< b) = ( TypeOf a, TypeOf b )
  type TypeOf 'TyUnit = ()

  type TypeOf 'TyBool = Bool
  type TypeOf 'TyInt = Integer
  type TypeOf 'TyString = String


  type SymbOf (a ':-> b) = SymbOf a -> SymbOf b
  type SymbOf (a ':>< b) = ( SymbOf a, SymbOf b )
  type SymbOf 'TyUnit = ()

  type SymbOf 'TyBool = SBool
  type SymbOf 'TyInt = SInteger
  type SymbOf 'TyString = SString



-- Primitives ------------------------------------------------------------------


data IsPrim (a :: Ty) where
  BoolIsPrim :: IsPrim 'TyBool
  IntIsPrim :: IsPrim 'TyInt
  StringIsPrim :: IsPrim 'TyString
