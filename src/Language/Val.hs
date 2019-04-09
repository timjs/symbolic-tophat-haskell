module Language.Val
  ( module Language.Types
  , Val(..), Un(..), Bn(..)
  , pattern B, pattern I, pattern S
  , Task(..)
  , pattern View, pattern (:&&:), pattern (:||:), pattern (:??:), pattern (:>>=), pattern (:>>?)
  , toPred
  ) where


import Language.Types
import Language.Ops

import qualified Language.Expr as E
import qualified Language.Pred as P



-- Expressions -----------------------------------------------------------------


data Val (cxt :: List Ty) (sxt :: List PrimTy) (t :: Ty) where
  Lam :: Val (a ': cxt) sxt b -> Val cxt sxt (a ':-> b)

  Sym :: HasType (t ': ts) a -> Val cxt (t ': ts) ('TyPrim a)
  Con :: IsPrim a -> ConcOf a -> Val cxt sxt ('TyPrim a)

  Un :: Un a b -> Val cxt sxt ('TyPrim a) -> Val cxt sxt ('TyPrim b)
  Bn :: Bn a b c -> Val cxt sxt ('TyPrim a) -> Val cxt sxt ('TyPrim b) -> Val cxt sxt ('TyPrim c)

  Unit :: Val cxt sxt 'TyUnit
  Pair :: Val cxt sxt a -> Val cxt sxt b -> Val cxt sxt (a ':>< b)

  Task :: Task cxt sxt ('TyTask a) -> Val cxt sxt ('TyTask a)


pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Val cxt sxt t) where
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



-- Tasks -----------------------------------------------------------------------


data Task (cxt :: List Ty)  (sxt :: List PrimTy) (t :: Ty) where
  Edit :: IsBasic a => Val cxt sxt a -> Task cxt sxt ('TyTask a)
  Enter :: IsBasic a => Task cxt sxt ('TyTask a)
  -- Store :: Loc a -> Task cxt sxt ('TyTask a)

  Fail :: Task cxt sxt ('TyTask a)

  And :: Val cxt sxt ('TyTask a) -> Val cxt sxt ('TyTask b) -> Task cxt sxt ('TyTask (a ':>< b))
  Or  :: Val cxt sxt ('TyTask a) -> Val cxt sxt ('TyTask a) -> Task cxt sxt ('TyTask a)
  Xor :: E.Expr cxt sxt ('TyTask a) -> E.Expr cxt sxt ('TyTask a) -> Task cxt sxt ('TyTask a)

  Then :: Val cxt sxt ('TyTask a) -> E.Expr cxt sxt (a ':-> 'TyTask b) -> Task cxt sxt ('TyTask b)
  Next :: Val cxt sxt ('TyTask a) -> E.Expr cxt sxt (a ':-> 'TyTask b) -> Task cxt sxt ('TyTask b)


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


instance Pretty (Task cxt sxt t) where
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



-- Translation -----------------------------------------------------------------

toPred :: Val '[] sxt ('TyPrim a) -> P.Pred sxt a
toPred = \case
  Sym i -> P.Sym i
  Con p x -> P.Con p x
  Un o v1 -> P.Un o (toPred v1)
  Bn o v1 v2 -> P.Bn o (toPred v1) (toPred v2)
