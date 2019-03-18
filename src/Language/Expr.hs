module Language.Expr
  ( module Language.Types
  , HasType(..)
  , Expr(..), Un(..), Bn(..)
  , pattern B, pattern I, pattern S
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

  -- | Symbolic variable
  -- |
  -- | Note we demand the symbolic context to be non-empty when using any symbol.
  Sym :: HasType (t ': ts) a -> Expr cxt (t ': ts) a
  Con :: IsPrim a -> TypeOf a -> Expr cxt sxt a

  Un :: Un a b -> Expr cxt sxt a -> Expr cxt sxt b
  Bn :: Bn a b c -> Expr cxt sxt a -> Expr cxt sxt b -> Expr cxt sxt c
  If :: Mergeable (SymbOf a) => Expr cxt sxt 'TyBool -> Expr cxt sxt a -> Expr cxt sxt a -> Expr cxt sxt a

  Unit :: Expr cxt sxt 'TyUnit
  Pair :: Expr cxt sxt a -> Expr cxt sxt b -> Expr cxt sxt (a ':>< b)
  Fst :: Expr cxt sxt (a ':>< b) -> Expr cxt sxt a
  Snd :: Expr cxt sxt (a ':>< b) -> Expr cxt sxt b


pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Expr cxt sxt t) where
  pretty = \case
    Lam f -> "λ." <> pretty f
    App f a -> sep [ parens (pretty f), parens (pretty a) ]
    Var i -> "x" <> pretty i
    Sym i -> "s" <> pretty i

    Con BoolIsPrim x -> pretty x
    Con IntIsPrim x -> pretty x
    Con StringIsPrim x -> pretty x

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])
    If p a b -> sep [ "if", pretty p, "then", pretty a, "else", pretty b ]

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b
    Fst a -> "fst" <+> pretty a
    Snd a -> "snd" <+> pretty a
