module Language.Expr
  ( module Language.Types
  , HasType(..)
  , Expr(..), Un(..), Bn(..)
  ) where


import Data.Text.Prettyprint.Doc

import Language.Types
import Language.Ops



-- Expressions -----------------------------------------------------------------


data Expr (cxt :: List Ty) (sxt :: List PrimTy) (t :: Ty) where
  Lam :: Expr (a ': cxt) sxt b -> Expr cxt sxt (a ':-> b)
  App :: Expr cxt sxt (a ':-> b) -> Expr cxt sxt a -> Expr cxt sxt b
  Var :: HasType cxt a -> Expr cxt sxt a
  Sym :: HasType sxt a -> Expr cxt sxt ('TyPrim a)
  Con :: IsBasic (TypeOf a) => TypeOf a -> Expr cxt sxt a

  Un :: Un a b -> Expr cxt sxt ('TyPrim a) -> Expr cxt sxt ('TyPrim b)
  Bn :: Bn a b c -> Expr cxt sxt ('TyPrim a) -> Expr cxt sxt ('TyPrim b) -> Expr cxt sxt ('TyPrim c)
  If :: Expr cxt sxt ('TyPrim 'TyBool) -> Expr cxt sxt a -> Expr cxt sxt a -> Expr cxt sxt a

  Unit :: Expr cxt sxt 'TyUnit
  Pair :: Expr cxt sxt a -> Expr cxt sxt b -> Expr cxt sxt (a ':>< b)
  Fst :: Expr cxt sxt (a ':>< b) -> Expr cxt sxt a
  Snd :: Expr cxt sxt (a ':>< b) -> Expr cxt sxt b


instance Pretty (Expr cxt sxt t) where
  pretty = \case
    Lam f -> "λ." <> pretty f
    App f a -> sep [ parens (pretty f), parens (pretty a) ]
    Var i -> "x" <> pretty i
    Sym i -> "s" <> pretty i
    Con a -> pretty a

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])
    If p a b -> sep [ "if", pretty p, "then", pretty a, "else", pretty b ]

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b
    Fst a -> "fst" <+> pretty a
    Snd a -> "snd" <+> pretty a
