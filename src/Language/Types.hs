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
  type TypeOf s (a ':-> b) = TypeOf s a -> TypeOf s b
  type TypeOf s (a ':>< b) = ( TypeOf s a, TypeOf s b )
  type TypeOf s 'TyUnit = ()

  type TypeOf 'Concrete 'TyBool = Bool
  type TypeOf 'Concrete 'TyInt = Integer
  type TypeOf 'Concrete 'TyString = String

  type TypeOf 'Symbolic 'TyBool = SBool
  type TypeOf 'Symbolic 'TyInt = SInteger
  type TypeOf 'Symbolic 'TyString = SString



-- Primitives ------------------------------------------------------------------


data IsPrim (a :: Ty) where
  BoolIsPrim :: IsPrim 'TyBool
  IntIsPrim :: IsPrim 'TyInt
  StringIsPrim :: IsPrim 'TyString
