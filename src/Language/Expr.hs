module Language.Expr
  ( module Language.Types
  , HasType(..), idx
  , Expr(..), Un(..), Bn(..)
  ) where


import Data.Text.Prettyprint.Doc

import Language.Types



-- Contexts --------------------------------------------------------------------


data HasType (cxt :: List Ty) (t :: Ty) where
  Here :: HasType (t ': ts) t
  There :: HasType ts t -> HasType (t2 ': ts) t


instance Pretty (HasType cxt t) where
  pretty = pretty << idx


idx :: HasType cxt t -> Int
idx = \case
  Here -> 0
  There xs -> 1 + idx xs



-- Operations ------------------------------------------------------------------

-- Unary -


data Un (a :: Ty) (b :: Ty) where
  Not :: Un 'TyBool 'TyBool

  Neg :: Un 'TyInt 'TyInt

  Fst :: Un (a ':>< b) a
  Snd :: Un (a ':>< b) b


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"
    Neg -> "neg"

    Fst -> "fst"
    Snd -> "snd"



-- Binary --


data Bn (a :: Ty) (b :: Ty) (c :: Ty) where
  And :: Bn 'TyBool 'TyBool 'TyBool
  Or  :: Bn 'TyBool 'TyBool 'TyBool

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


instance Pretty (Bn a b c) where
  pretty = \case
    And -> "&&"
    Or  -> "||"

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



-- Expressions -----------------------------------------------------------------


data Expr (cxt :: List Ty) (sxt :: List Ty) (t :: Ty) where
  Lam :: Expr (a ': cxt) sxt t -> Expr cxt sxt (a ':-> t)
  App :: Expr cxt sxt (a ':-> b) -> Expr cxt sxt a -> Expr cxt sxt b
  Var :: HasType cxt a -> Expr cxt sxt a
  Sym :: IsBasic (TypeOf a) => HasType sxt a -> Expr cxt sxt a
  Val :: IsBasic (TypeOf a) => TypeOf a -> Expr cxt sxt a

  Un :: Un a b -> Expr cxt sxt a -> Expr cxt sxt b
  Bn :: Bn a b c -> Expr cxt sxt a -> Expr cxt sxt b -> Expr cxt sxt c
  If :: Expr cxt sxt 'TyBool -> Expr cxt sxt a -> Expr cxt sxt a -> Expr cxt sxt a

  Unit :: Expr cxt sxt 'TyUnit
  Pair :: Expr cxt sxt a -> Expr cxt sxt b -> Expr cxt sxt (a ':>< b)


instance Pretty (Expr cxt sxt t) where
  pretty = \case
    Lam f -> "λ." <> pretty f
    App f a -> sep [parens (pretty f), parens (pretty a)]
    Var i -> "x" <> pretty i
    Sym i -> "s" <> pretty i
    Val a -> pretty a

    Un o a -> parens (sep [pretty o, pretty a])
    Bn o a b -> parens (sep [pretty a, pretty o, pretty b])
    If p a b -> sep ["if", pretty p, "then", pretty a, "else", pretty b]

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b
