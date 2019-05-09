module Language.Expr
  ( module Language.Types
  , module Language.Names
  , Expr(..), Un(..), Bn(..)
  , pattern B, pattern I, pattern S
  , Pretask(..)
  , pattern View, pattern (:&&:), pattern (:||:), pattern (:??:), pattern (:>>=), pattern (:>>?)
  ) where


import Data.Editable

import Language.Names
import Language.Types
import Language.Ops



-- Expressions -----------------------------------------------------------------


data Expr (t :: Ty) where
  -- | FIXME: Explain why Typeable.
  Lam :: Typeable a => Expr b -> Expr (a ':-> b)
  App :: Expr (a ':-> b) -> Expr a -> Expr b
  Var :: Typeable a => Name a -> Expr a

  Sym :: Name ('TyPrim a) -> Expr ('TyPrim a)
  Con :: IsPrim a -> TypeOf a -> Expr ('TyPrim a)

  Un :: Un a b -> Expr ('TyPrim a) -> Expr ('TyPrim b)
  Bn :: Bn a b c -> Expr ('TyPrim a) -> Expr ('TyPrim b) -> Expr ('TyPrim c)
  If :: Expr ('TyPrim 'TyBool) -> Expr a -> Expr a -> Expr a

  Unit :: Expr 'TyUnit
  Pair :: Expr a -> Expr b -> Expr (a ':>< b)
  Fst :: Expr (a ':>< b) -> Expr a
  Snd :: Expr (a ':>< b) -> Expr b

  Task :: Pretask ('TyTask a) -> Expr ('TyTask a)


pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Expr t) where
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

    Task p -> pretty p



-- Pretasks --------------------------------------------------------------------


data Pretask (t :: Ty) where
  Edit :: Editable (TypeOf a) => Expr a -> Pretask ('TyTask a)
  Enter :: Editable (TypeOf a) => Pretask ('TyTask a)
  -- Store :: Loc a -> Pretask ('TyTask a)

  Fail :: Pretask ('TyTask a)

  And :: Expr ('TyTask a) -> Expr ('TyTask b) -> Pretask ('TyTask (a ':>< b))
  Or  :: Expr ('TyTask a) -> Expr ('TyTask a) -> Pretask ('TyTask a)
  Xor :: Expr ('TyTask a) -> Expr ('TyTask a) -> Pretask ('TyTask a)

  Then :: Expr ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Pretask ('TyTask b)
  Next :: Expr ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Pretask ('TyTask b)


infixl 3 :&&:
infixr 2 :||:, :??:
-- | NOTE:
-- | Fixity of bind is left associative in a normal setting because of the scoping of lambdas.
-- | Because we can't use lambdas in our DSL, bind should be right associative.
infixr 1 :>>=, :>>?


pattern View x = Edit x
pattern (:&&:) x y = And (Task x) (Task y)
pattern (:||:) x y = Or (Task x) (Task y)
pattern (:??:) x y = Xor (Task x) (Task y)
pattern (:>>=) t c = Then (Task t) (Lam (Task c))
pattern (:>>?) t c = Next (Task t) (Lam (Task c))


instance Pretty (Pretask t) where
  pretty = \case
    Edit x -> cat [ "□(", pretty x, ")" ]
    Enter -> "□(_)"
    -- Store -> "■(_)"
    And x y -> sep [ pretty x, "⋈", pretty y ]
    Or x y -> sep [ pretty x, "◆", pretty y ]
    Xor x y -> sep [ pretty x, "◇", pretty y ]
    Fail -> "↯"
    Then x c -> sep [ pretty x, "▶", pretty c ]
    Next x c -> sep [ pretty x, "▷…", pretty c ]
