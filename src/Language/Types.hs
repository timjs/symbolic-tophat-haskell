module Language.Types
  ( module Data.Universe
  , Ty(..), PrimTy(..)
  -- , TyPrimBool, TyPrimInt, TyPrimString
  , IsPrim(..), IsBasic
  ) where


import Data.Basic
import Data.Universe

import Data.SBV (SBV)



-- Universe --------------------------------------------------------------------


data {- kind -} Ty
  = Ty :-> Ty
  | Ty :>< Ty
  | TyUnit
  | TyTask Ty
  | TyPrim PrimTy


instance Universe Ty where
  type TypeOf s (a ':-> b) = TypeOf s a -> TypeOf s b
  type TypeOf s (a ':>< b) = ( TypeOf s a, TypeOf s b )
  type TypeOf _ 'TyUnit = ()
  -- type TypeOf s ('TyTask a) = Task (TypeOf s a)

  type TypeOf s ('TyPrim p) = TypeOf s p

  -- type TypeOf 'Concrete ('TyPrim 'TyBool) = Bool
  -- type TypeOf 'Concrete ('TyPrim 'TyInt) = Integer
  -- type TypeOf 'Concrete ('TyPrim 'TyString) = String

  -- type TypeOf 'Symbolic ('TyPrim 'TyBool) = SBool
  -- type TypeOf 'Symbolic ('TyPrim 'TyInt) = SInteger
  -- type TypeOf 'Symbolic ('TyPrim 'TyString) = SString



-- Primitives ------------------------------------------------------------------


data PrimTy
  = TyBool
  | TyInt
  | TyString


-- type TyPrimBool = TyPrim TyBool
-- type TyPrimInt = TyPrim TyInt
-- type TyPrimString = TyPrim TyString


instance Universe PrimTy where
  type TypeOf 'Concrete 'TyBool = Bool
  type TypeOf 'Concrete 'TyInt = Integer
  type TypeOf 'Concrete 'TyString = String

  -- | This needs undecidable instances...
  -- type TypeOf 'Symbolic a = SBV (TypeOf 'Concrete a)

  type TypeOf 'Symbolic 'TyBool = SBV Bool
  type TypeOf 'Symbolic 'TyInt = SBV Integer
  type TypeOf 'Symbolic 'TyString = SBV String


-- | Proof that some `PrimTy` is primitive.
-- |
-- | We need this at runtime to check what kind of primitive we have.
-- | Apparently we can't use the dictionary trick as with `IsBasic`
-- | due to some strange interaction with type families...
data IsPrim (a :: PrimTy) where
  BoolIsPrim :: IsPrim 'TyBool
  IntIsPrim :: IsPrim 'TyInt
  StringIsPrim :: IsPrim 'TyString


-- class ( Basic (ConcOf a) ) => IsPrim (a :: PrimTy)
--
-- instance IsPrim 'TyBool
-- instance IsPrim 'TyInt
-- instance IsPrim 'TyString



-- Basics ----------------------------------------------------------------------


class ( Basic (ConcOf a) ) => IsBasic (a :: Ty)

instance IsBasic ('TyPrim 'TyBool)
instance IsBasic ('TyPrim 'TyInt)
instance IsBasic ('TyPrim 'TyString)

instance IsBasic 'TyUnit
instance ( IsBasic a, IsBasic b ) => IsBasic (a ':>< b)
