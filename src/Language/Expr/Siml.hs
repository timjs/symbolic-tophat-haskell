module Language.Expr.Siml where

import Language.Names
import Language.Types

import Language.Expr (Expr, Pretask)
import Language.Pred (Pred, pattern Yes, pattern (:/\:))
import Language.Val (Val, asPred, asExpr)

import qualified Language.Expr as E
import qualified Language.Pred as P
import qualified Language.Val as V


--------------------------------------------------------------------------------

-- Substitution --


subst
  :: Typeable a
  => Name a -- ^ Substitute a name of type `a`
  -> Expr a -- ^ with expression of type `a`
  -> Expr b -- ^ in an expression of type `b` containing a variable of type `a`
  -> Expr b -- ^ giving the modified expression of type `b`.
subst j s = \case
  E.Lam e -> E.Lam (subst (j + 1) (shift 0 s) e)
  E.App f a -> E.App (subst j s f) (subst j s a)
  E.Var i
    | Just Refl <- i ~= j, i == j -> s
    | otherwise -> E.Var i

  E.Un o a -> E.Un o (subst j s a)
  E.Bn o a b -> E.Bn o (subst j s a) (subst j s b)
  E.If p a b -> E.If (subst j s p) (subst j s a) (subst j s b)
  E.Pair a b -> E.Pair (subst j s a) (subst j s b)
  E.Fst a -> E.Fst (subst j s a)
  E.Snd a -> E.Snd (subst j s a)
  E.Task p -> E.Task (subst' j s p)

  E.Sym i -> E.Sym i
  E.Con p x -> E.Con p x
  E.Unit -> E.Unit


subst' :: Typeable a => Name a -> Expr a -> Pretask b -> Pretask b
subst' j s = \case
  E.Edit x -> E.Edit (subst j s x)
  E.Enter -> E.Enter
  -- Store ->
  E.And x y -> E.And (subst j s x) (subst j s y)
  E.Or x y -> E.Or (subst j s x) (subst j s y)
  E.Xor x y -> E.Xor (subst j s x) (subst j s y)
  E.Fail -> E.Fail
  E.Then x c -> E.Then (subst j s x) (subst j s c)
  E.Next x c -> E.Next (subst j s x) (subst j s c)


-- -- | The one-place shift of an expression `e` after cutof `c`.
-- shift :: forall a b. Name a -> Expr b -> Expr b
-- shift c = \case
--   E.Var i
--     | Just Refl <- ra ~~ rb -> if
--       | i <  c -> E.Var i
--       | i >= c -> E.Var (i + 1)
--   E.App f a -> E.App (shift c f) (shift c a)
--   where
--     ra = typeRep :: TypeRep a
--     rb = typeRep :: TypeRep b


shift :: Name a -> Expr b -> Expr b
shift c@(Name j) = \case
  E.Var (Name i)
    | i <  j -> E.Var (Name i)
    | i >= j -> E.Var (Name $ i + 1)
  E.App f a -> E.App (shift c f) (shift c a)







-- Semantics -------------------------------------------------------------------

{- | Evaluate an expression symbolicaly.

Returns a list of all possible values after evaluation
combined with the predicate which has to hold to get that value.
Note that the context of symbolic values `sxt` is the same for the expression
and the resulting predicate.
-}
eval :: Expr t -> List ( Val t, Pred 'TyBool )
eval = \case
  E.App e1 e2 -> do
    ( V.Lam e1', p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    ( v1, p3 ) <- eval $ subst 0 (asExpr v2) e1'
    pure ( v1, p1 :/\: p2 :/\: p3 )

  E.Un o e1 -> do
    ( v1, p1 ) <- eval e1
    pure ( V.Un o v1, p1 )
  E.Bn o e1 e2 -> do
    ( v1, p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    pure ( V.Bn o v1 v2, p1 :/\: p2 )
  E.If e1 e2 e3 -> do
    ( v1, p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    ( v3, p3 ) <- eval e3
    [ ( v2, p1 :/\: p2 :/\: asPred v1 ), ( v3, p1 :/\: p3 :/\: P.Not (asPred v1) ) ]

  E.Pair e1 e2 -> do
    ( v1, p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    pure ( V.Pair v1 v2, p1 :/\: p2 )
  E.Fst e -> do --FIXME: missing
    ( V.Pair v _, p ) <- eval e
    pure ( v, p )
  E.Snd e -> do --FIXME: missing
    ( V.Pair _ v, p ) <- eval e
    pure ( v, p )

  E.Task x ->
    undefined

  E.Lam e ->
    [ ( V.Lam e, Yes ) ]
  E.Sym i ->
    [ ( V.Sym i, Yes ) ]
  E.Con p x ->
    [ ( V.Con p x, Yes ) ]
  E.Unit ->
    [ ( V.Unit, Yes )]
