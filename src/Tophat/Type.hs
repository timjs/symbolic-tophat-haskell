module Tophat.Type
  ( module Data.Universe
  , Ty(..), PrimTy(..)
  , TyPrimUnit, TyPrimBool, TyPrimInt, TyPrimString
  , Editable
  ) where

import Data.SBV
import Data.Universe


-- Types -----------------------------------------------------------------------

-- | *Kind* to index the meta types of values and expressions.
data Ty
  = Ty :-> Ty
  | Ty :>< Ty
  | TyTask Ty
  | TyRef PrimTy
  | TyPrim PrimTy

instance Universe Ty where
  type TypeOf (a ':-> b) = TypeOf a -> TypeOf b
  type TypeOf (a ':>< b) = ( TypeOf a, TypeOf b )
  -- type TypeOf ('TyTask a) = Task (TypeOf a)
  -- type TypeOf ('TyRef p) = Ref (TypeOf a)
  type TypeOf ('TyPrim p) = TypeOf p


-- Primitive types -------------------------------------------------------------

data PrimTy
  = TyUnit
  | TyBool
  | TyInt
  | TyString

instance Universe PrimTy where
  type TypeOf 'TyUnit = ()
  type TypeOf 'TyBool = Bool
  type TypeOf 'TyInt = Integer
  type TypeOf 'TyString = String

type TyPrimUnit = 'TyPrim 'TyUnit
type TyPrimBool = 'TyPrim 'TyBool
type TyPrimInt = 'TyPrim 'TyInt
type TyPrimString = 'TyPrim 'TyString


-- Editable types --------------------------------------------------------------

class ( Typeable (TypeOf a), Pretty (TypeOf a), Eq (TypeOf a), SymVal (TypeOf a) ) => Editable a

instance Editable 'TyUnit
instance Editable 'TyBool
instance Editable 'TyInt
instance Editable 'TyString
