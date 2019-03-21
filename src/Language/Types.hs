module Language.Types
  ( module Data.Universe
  , Ty(..), IsPrim(..), IsBasic
  ) where


import Data.Basic
import Data.Universe

import Data.SBV (SBool, SInteger, SString)



-- Universe --------------------------------------------------------------------


data {- kind -} Ty
  = Ty :-> Ty

  | TyTask Ty

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



-- Primitives & Basics ---------------------------------------------------------


data IsPrim (a :: Ty) where
  BoolIsPrim :: IsPrim 'TyBool
  IntIsPrim :: IsPrim 'TyInt
  StringIsPrim :: IsPrim 'TyString


-- data IsBasic (a :: Ty) where
--   PairIsBasic :: IsBasic a -> IsBasic b -> IsBasic (a ':>< b)
--   UnitIsBasic :: IsBasic 'TyUnit
--   BoolIsBasic :: IsBasic 'TyBool
--   IntIsBasic :: IsBasic 'TyInt
--   StringIsBasic :: IsBasic 'TyString


class ( Basic (ConcOf a) ) => IsBasic (a :: Ty)

instance IsBasic 'TyBool
instance IsBasic 'TyInt
instance IsBasic 'TyString
instance IsBasic 'TyUnit
instance ( IsBasic a, IsBasic b ) => IsBasic (a ':>< b)
