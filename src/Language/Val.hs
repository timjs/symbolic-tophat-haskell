module Language.Val
  ( module Language.Type
  , Val(..), Un(..), Bn(..)
  , pattern B, pattern I, pattern S
  , Task(..)
  , pattern View, pattern (:&&:), pattern (:||:), pattern (:??:), pattern (:>>=), pattern (:>>?)
  , asPred, asExpr, asPretask
  ) where

-- | NOTE:
-- | If we were programming in a dependently typed language,
-- | We could make a predicate `IsVal` over `Expr`.
-- | Alas, here we use a separate data type to keep values and expressions
-- | distinct.

import Data.Editable
import Language.Type
import Language.Name
import Language.Op

import Language.Expr (Expr)

import qualified Language.Expr as E
import qualified Language.Pred as P



-- Expressions -----------------------------------------------------------------


data Val (t :: Ty) where
  Lam :: Typeable a => Expr b -> Val (a ':-> b)

  Sym :: Name ('TyPrim a) -> Val ('TyPrim a)
  Con :: IsPrim a -> TypeOf a -> Val ('TyPrim a)

  Un :: Un a b -> Val ('TyPrim a) -> Val ('TyPrim b)
  Bn :: Bn a b c -> Val ('TyPrim a) -> Val ('TyPrim b) -> Val ('TyPrim c)

  Unit :: Val 'TyUnit
  Pair :: Val a -> Val b -> Val (a ':>< b)

  Task :: Task ('TyTask a) -> Val ('TyTask a)


pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Val t) where
  pretty = \case
    Lam f -> "λ." <> pretty f
    Sym i -> "s" <> pretty i

    Con BoolIsPrim x -> pretty x
    Con IntIsPrim x -> pretty x
    Con StringIsPrim x -> pretty x

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b

    Task p -> pretty p


instance Eq (Val t) where
  _ == _ = undefined



-- Tasks -----------------------------------------------------------------------


data Task (t :: Ty) where
  Edit :: Editable (TypeOf a) => Val a -> Task ('TyTask a)
  Enter :: Editable (TypeOf a) => Task ('TyTask a)
  -- Store :: Loc a -> Task ('TyTask a)

  Fail :: Task ('TyTask a)

  And :: Val ('TyTask a) -> Val ('TyTask b) -> Task ('TyTask (a ':>< b))
  Or  :: Val ('TyTask a) -> Val ('TyTask a) -> Task ('TyTask a)
  Xor :: Expr ('TyTask a) -> Expr ('TyTask a) -> Task ('TyTask a)

  Then :: Val ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Task ('TyTask b)
  Next :: Val ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Task ('TyTask b)


infixl 3 :&&:
infixr 2 :||:, :??:
-- | NOTE:
-- | Fixity of bind is left associative in a normal setting because of the scoping of lambdas.
-- | Because we can't use lambdas in our DSL, bind should be right associative.
infixr 1 :>>=, :>>?


pattern View x = Edit x
pattern (:&&:) x y = And (Task x) (Task y)
pattern (:||:) x y = Or (Task x) (Task y)
pattern (:??:) x y = Xor (E.Task x) (E.Task y)
pattern (:>>=) t c = Then (Task t) (E.Lam (E.Task c))
pattern (:>>?) t c = Next (Task t) (E.Lam (E.Task c))


instance Pretty (Task t) where
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


instance Eq (Task t) where
  _ == _ = undefined



-- Translation -----------------------------------------------------------------


asPred :: Val ('TyPrim a) -> P.Pred a
asPred = \case
  Sym i -> P.Sym i
  Con p x -> P.Con p x

  Un o v1 -> P.Un o (asPred v1)
  Bn o v1 v2 -> P.Bn o (asPred v1) (asPred v2)


asExpr :: Val a -> Expr a
asExpr = \case
  Lam f -> E.Lam f
  Sym i -> E.Sym i
  Con p x -> E.Con p x

  Un o a -> E.Un o (asExpr a)
  Bn o a b -> E.Bn o (asExpr a) (asExpr b)

  Unit -> E.Unit
  Pair a b -> E.Pair (asExpr a) (asExpr b)

  Task p -> E.Task (asPretask p)


asPretask :: Task a -> E.Pretask a
asPretask = \case
  Edit x -> E.Edit (asExpr x)
  Enter -> E.Enter
  -- Store ->
  And x y -> E.And (asExpr x) (asExpr y)
  Or x y -> E.Or (asExpr x) (asExpr y)
  Xor x y -> E.Xor x y
  Fail -> E.Fail
  Then x c -> E.Then (asExpr x) c
  Next x c -> E.Next (asExpr x) c
