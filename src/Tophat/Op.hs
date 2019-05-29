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
  Not :: Un 'TyBool 'TyBool
  Neg :: Un 'TyInt  'TyInt

  Len   :: ( Editable p ) => Un ('TyList p) 'TyInt
  Empty :: ( Editable p ) => Un ('TyList p) 'TyBool
  Uniq  :: ( Editable p ) => Un ('TyList p) 'TyBool


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"
    Neg -> "neg"

    Len   -> "len"
    Empty -> "empty"
    Uniq  -> "uniq"


instance Eq (Un a b) where
  Not == Not = True
  Neg == Neg = True

  Len   == Len   = True
  Empty == Empty = True
  Empty == _     = False
  Uniq  == Uniq  = True
  Uniq  == _     = False


-- Binary --

data Bn (a :: PrimTy) (b :: PrimTy) (c :: PrimTy) where
  Conj :: Bn 'TyBool 'TyBool 'TyBool
  Disj :: Bn 'TyBool 'TyBool 'TyBool

  Lt :: Bn 'TyInt 'TyInt 'TyBool
  Le :: Bn 'TyInt 'TyInt 'TyBool
  Eq :: Bn 'TyInt 'TyInt 'TyBool
  Nq :: Bn 'TyInt 'TyInt 'TyBool
  Ge :: Bn 'TyInt 'TyInt 'TyBool
  Gt :: Bn 'TyInt 'TyInt 'TyBool

  Add :: Bn 'TyInt 'TyInt 'TyInt
  Sub :: Bn 'TyInt 'TyInt 'TyInt
  Mul :: Bn 'TyInt 'TyInt 'TyInt
  Div :: Bn 'TyInt 'TyInt 'TyInt

  Elem :: ( Editable p ) => Bn ('TyList p) ('TyList p) 'TyBool
  Cat  :: ( Editable p ) => Bn ('TyList p) ('TyList p) ('TyList p)


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

    Len   -> svLen   (Proxy :: Proxy a)
    Empty -> svEmpty (Proxy :: Proxy a)
    Uniq  -> svUniq  (Proxy :: Proxy a)


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


-- | Note: I don't know why the Proxy argument lifts some ambiguity.
-- | Without it GHC complains...
svEmpty :: forall p. Editable p => Proxy ('TyList p) -> SVal -> SVal
svEmpty _ xs = unSBV $ Smt.null (SBV xs :: SList (TypeOf p))

svLen :: forall p. Editable p => Proxy ('TyList p) -> SVal -> SVal
svLen _ xs = unSBV $ Smt.length (SBV xs :: SList (TypeOf p))

svUniq :: forall p. Editable p => Proxy ('TyList p) -> SVal -> SVal
svUniq _ xs = unSBV $ uniq (SBV xs :: SList (TypeOf p))
  where
    uniq :: Eq a => SymVal a => SList a -> SBool
    uniq l = ite (Smt.null l)
      (sTrue)
      (let ( hd, tl ) = Smt.uncons l in hd `Smt.notElem` tl .&& uniq tl)

svElem :: forall p. Editable p => Proxy ('TyList p) -> SVal -> SVal -> SVal
svElem _ x xs = unSBV $ (SBV x :: SBV (TypeOf p)) `Smt.elem` (SBV xs :: SList (TypeOf p))

svCat :: forall p. Editable p => Proxy ('TyList p) -> SVal -> SVal -> SVal
svCat _ xs ys = unSBV $ Smt.concat (SBV xs :: SList (TypeOf p)) (SBV ys :: SList (TypeOf p))
