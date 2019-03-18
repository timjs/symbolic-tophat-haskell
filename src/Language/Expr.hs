module Language.Expr
  ( module Language.Types
  , HasType(..)
  , Expr(..), Un(..), Bn(..)
  ) where


import Data.SBV
import Data.Text.Prettyprint.Doc

import Language.Types
import Language.Ops



-- Expressions -----------------------------------------------------------------


data Expr (cxt :: List Ty) (sxt :: List Ty) (t :: Ty) where
  Lam :: Expr (a ': cxt) sxt b -> Expr cxt sxt (a ':-> b)
  App :: Expr cxt sxt (a ':-> b) -> Expr cxt sxt a -> Expr cxt sxt b
  Var :: HasType cxt a -> Expr cxt sxt a
  Sym :: HasType sxt a -> Expr cxt sxt a

  -- Con :: IsBasic (TypeOf a) => TypeOf a -> Expr cxt sxt a
  I :: Integer -> Expr cxt sxt 'TyInt
  B :: Bool -> Expr cxt sxt 'TyBool
  S :: String -> Expr cxt sxt 'TyString

  Un :: Un a b -> Expr cxt sxt a -> Expr cxt sxt b
  Bn :: Bn a b c -> Expr cxt sxt a -> Expr cxt sxt b -> Expr cxt sxt c
  If :: Mergeable (SymbOf a) => Expr cxt sxt 'TyBool -> Expr cxt sxt a -> Expr cxt sxt a -> Expr cxt sxt a

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

    -- Con x -> pretty x
    B x -> pretty x
    I x -> pretty x
    S x -> pretty x

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])
    If p a b -> sep [ "if", pretty p, "then", pretty a, "else", pretty b ]

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b
    Fst a -> "fst" <+> pretty a
    Snd a -> "snd" <+> pretty a
