module Language.Type
  ( module Data.Universe
  , Ty(..), PrimTy(..)
  , TyPrimUnit, TyPrimBool, TyPrimInt, TyPrimString
  , IsPrim(..)
  ) where

import Data.Universe


data {- kind -} Ty
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

-- | Proof that some `PrimTy` is primitive.
-- |
-- | We need this at runtime to check what kind of primitive we have.
-- | Apparently we can't use the dictionary trick as with `IsBasic`
-- | due to some strange interaction with type families...
data IsPrim (a :: PrimTy) where
  UnitIsPrim :: IsPrim 'TyUnit
  BoolIsPrim :: IsPrim 'TyBool
  IntIsPrim :: IsPrim 'TyInt
  StringIsPrim :: IsPrim 'TyString
