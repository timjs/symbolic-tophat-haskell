module Tophat.Type
  ( module Data.Universe
  , Ty(..), PrimTy(..)
  , TyUnit, TyBool, TyInt, TyString, TyPair, TyList
  , TyUnitUnit, TyUnitInt, TyIntUnit, TyIntInt, TyIntString, TyStringInt, TyIntList, TyStringList
  , Editable
  ) where

import Data.SBV
import Data.Universe


-- Types -----------------------------------------------------------------------

-- | *Kind* to index the meta types of values and expressions.
data Ty
  = Ty :-> Ty
  | TyTask Ty
  | TyRef PrimTy
  | TyPrim PrimTy

instance Universe Ty where
  type TypeOf (a ':-> b) = TypeOf a -> TypeOf b
  -- type TypeOf ('TyTask a) = Task (TypeOf a)
  -- type TypeOf ('TyRef p) = Ref (TypeOf a)
  type TypeOf ('TyPrim p) = TypeOf p


-- Primitive types -------------------------------------------------------------

-- | Primitive types
-- |
-- | By specifying that certain expressions have a primitive type,
-- | the compiler can help us to keep our functions total.
data PrimTy
  = TyPrimUnit
  | TyPrimBool
  | TyPrimInt
  | TyPrimString
  | TyPrimPair PrimTy PrimTy
  | TyPrimList PrimTy

instance Universe PrimTy where
  type TypeOf 'TyPrimUnit = ()
  type TypeOf 'TyPrimBool = Bool
  type TypeOf 'TyPrimInt = Integer
  type TypeOf 'TyPrimString = String
  type TypeOf ('TyPrimPair p q) = ( TypeOf p, TypeOf q )
  type TypeOf ('TyPrimList p) = List (TypeOf p)

type TyUnit     = 'TyPrim 'TyPrimUnit
type TyBool     = 'TyPrim 'TyPrimBool
type TyInt      = 'TyPrim 'TyPrimInt
type TyString   = 'TyPrim 'TyPrimString
type TyPair a b = 'TyPrim ('TyPrimPair a b)
type TyList a   = 'TyPrim ('TyPrimList a)

type TyUnitUnit   = TyPair 'TyPrimUnit   'TyPrimUnit
type TyUnitInt    = TyPair 'TyPrimUnit   'TyPrimInt
type TyIntUnit    = TyPair 'TyPrimInt    'TyPrimUnit
type TyIntInt     = TyPair 'TyPrimInt    'TyPrimInt
type TyStringInt  = TyPair 'TyPrimString 'TyPrimInt
type TyIntString  = TyPair 'TyPrimInt    'TyPrimString
type TyIntList    = TyList 'TyPrimInt
type TyStringList = TyList 'TyPrimString


-- Editable types --------------------------------------------------------------

class ( Typeable (TypeOf a), Pretty (TypeOf a), Eq (TypeOf a), SymVal (TypeOf a) ) => Editable a

instance Editable 'TyPrimUnit
instance Editable 'TyPrimBool
instance Editable 'TyPrimInt
instance Editable 'TyPrimString
instance ( Editable a, Editable b ) => Editable ('TyPrimPair a b)
instance ( Editable a ) => Editable ('TyPrimList a)
