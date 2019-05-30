module Tophat.Op
  ( module Tophat.Type
  , Un(..), Bn(..)
  , toSmtUn, toSmtBn
  ) where

import Data.SBV.Dynamic
import Tophat.Type

import Data.SBV (SymVal, SBool, SList, ite, sTrue, (.&&))
import Data.SBV.Internals (SBV(..))

import qualified Data.SBV.List as Smt


-- Operations ------------------------------------------------------------------

-- Unary -

data Un (a :: PrimTy) (b :: PrimTy) where
  Not :: Un 'TyPrimBool 'TyPrimBool
  Neg :: Un 'TyPrimInt  'TyPrimInt

  Len  :: ( Editable p ) => Un ('TyPrimList p) 'TyPrimInt
  Uniq :: ( Editable p ) => Un ('TyPrimList p) 'TyPrimBool


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"
    Neg -> "neg"

    Len  -> "len"
    Uniq -> "uniq"


instance Eq (Un a b) where
  Not == Not = True
  Neg == Neg = True

  Len  == Len  = True
  Uniq == Uniq = True


-- Binary --

data Bn (a :: PrimTy) (b :: PrimTy) (c :: PrimTy) where
  Conj :: Bn 'TyPrimBool 'TyPrimBool 'TyPrimBool
  Disj :: Bn 'TyPrimBool 'TyPrimBool 'TyPrimBool

  Lt :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimBool
  Le :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimBool
  Eq :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimBool
  Nq :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimBool
  Ge :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimBool
  Gt :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimBool

  Add :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimInt
  Sub :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimInt
  Mul :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimInt
  Div :: Bn 'TyPrimInt 'TyPrimInt 'TyPrimInt

  Elem :: ( Editable p ) => Bn ('TyPrimList p) ('TyPrimList p) 'TyPrimBool
  Cat  :: ( Editable p ) => Bn ('TyPrimList p) ('TyPrimList p) ('TyPrimList p)


instance Pretty (Bn a b c) where
  pretty = \case
    Conj -> "∧"
    Disj -> "∨"

    Lt -> "<"
    Le -> "<="
    Eq -> "=="
    Nq -> "/="
    Ge -> ">="
    Gt -> ">"

    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"

    Elem -> "@"
    Cat  -> "++"


-- | Note: This strangely written definition is put together in such a way that
-- | new patterns will result in a warning.
instance Eq (Bn a b c) where
    Conj == Conj = True
    Conj == _    = False
    Disj == Disj = True
    Disj == _    = False

    Lt   == Lt = True
    Lt   == _  = False
    Le   == Le = True
    Le   == _  = False
    Eq   == Eq = True
    Eq   == _  = False
    Nq   == Nq = True
    Nq   == _  = False
    Ge   == Ge = True
    Ge   == _  = False
    Gt   == Gt = True
    Gt   == _  = False

    Add  == Add = True
    Add  == _   = False
    Sub  == Sub = True
    Sub  == _   = False
    Mul  == Mul = True
    Mul  == _   = False
    Div  == Div = True
    Div  == _   = False

    Elem == Elem = True
    Cat  == Cat  = True


-- Conversion ------------------------------------------------------------------

toSmtUn :: forall a b. Un a b -> SVal -> SVal
toSmtUn = \case
    Not -> svNot
    Neg -> svUNeg

    Len  -> svLen   (Proxy :: Proxy a)
    Uniq -> svUniq  (Proxy :: Proxy a)


toSmtBn :: forall a b c. Bn a b c -> SVal -> SVal -> SVal
toSmtBn = \case
    Conj -> svAnd
    Disj -> svOr

    Lt -> svLessThan
    Le -> svLessEq
    Eq -> svEqual
    Nq -> svNotEqual
    Ge -> svGreaterThan
    Gt -> svGreaterEq

    Add -> svPlus
    Sub -> svMinus
    Mul -> svTimes
    Div -> svDivide

    Elem -> svElem (Proxy :: Proxy b)
    Cat  -> svCat  (Proxy :: Proxy b)


-- | Note: The Proxy argument is there to lift the ambiguity of type variable `p`.
svLen :: forall p. Editable p => Proxy ('TyPrimList p) -> SVal -> SVal
svLen _ xs = unSBV $ Smt.length (SBV xs :: SList (TypeOf p))

svUniq :: forall p. Editable p => Proxy ('TyPrimList p) -> SVal -> SVal
svUniq _ xs = unSBV $ uniq (SBV xs :: SList (TypeOf p))
  where
    uniq :: Eq a => SymVal a => SList a -> SBool
    uniq l = ite (Smt.null l)
      (sTrue)
      (let ( hd, tl ) = Smt.uncons l in hd `Smt.notElem` tl .&& uniq tl)

svElem :: forall p. Editable p => Proxy ('TyPrimList p) -> SVal -> SVal -> SVal
svElem _ x xs = unSBV $ (SBV x :: SBV (TypeOf p)) `Smt.elem` (SBV xs :: SList (TypeOf p))

svCat :: forall p. Editable p => Proxy ('TyPrimList p) -> SVal -> SVal -> SVal
svCat _ xs ys = unSBV $ Smt.concat (SBV xs :: SList (TypeOf p)) (SBV ys :: SList (TypeOf p))
