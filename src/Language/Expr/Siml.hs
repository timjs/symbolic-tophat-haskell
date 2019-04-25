module Language.Expr.Siml where

import Language.Types

import Language.Expr (Expr, Pretask)
import Language.Pred (Pred, pattern Yes, pattern (:/\:))
import Language.Val (Val, asPred, asExpr)

import qualified Language.Expr as E
import qualified Language.Pred as P
import qualified Language.Val as V
import qualified Language.Ops as O


--------------------------------------------------------------------------------

-- Substitution --

{-
• Could not deduce: a1 ~ a
  from the context: t ~ (a1 ':-> b)
    bound by a pattern with constructor:
               E.Lam :: forall (a :: Ty) (cxt :: List Ty) (sxt :: List PrimTy) (b :: Ty).
                        Expr (a : cxt) sxt b -> Expr cxt sxt (a ':-> b),
             in a case alternative
  ‘a1’ is a rigid type variable bound by
    a pattern with constructor:
      E.Lam :: forall (a :: Ty) (cxt :: List Ty) (sxt :: List PrimTy) (b :: Ty).
               Expr (a : cxt) sxt b -> Expr cxt sxt (a ':-> b),
    in a case alternative
  ‘a’ is a rigid type variable bound by
    the type signature for:
      subst :: forall (a :: Ty) (cxt :: [Ty]) (sxt :: List PrimTy) (t :: Ty).
               HasType (a : cxt) a -> Expr cxt sxt a -> Expr (a : cxt) sxt t -> Expr cxt sxt t
  Expected type: Expr (a : a : cxt) sxt b
    Actual type: Expr (a1 : a : cxt) sxt b
• In the third argument of ‘subst’, namely ‘e’
  In the first argument of ‘E.Lam’, namely ‘(subst (There j) (shift s) e)’
  In the expression: E.Lam (subst (There j) (shift s) e)
• Relevant bindings include
    j :: HasType (a : cxt) a
    j' :: HasType (a : a : cxt) a
    s :: Expr cxt sxt a
    s' :: Expr (a : cxt) sxt a
    e :: Expr (a1 : a : cxt) sxt b
    subst :: HasType (a : cxt) a -> Expr cxt sxt a -> Expr (a : cxt) sxt t -> Expr cxt sxt t

    j' :: HasType (x : a : cxt) a
    s' :: Expr (y : cxt) sxt a
    e :: Expr (z : a : cxt) sxt b
    subst @j' @s' @e :: HasType (x : a : cxt) a -> Expr (y : cxt) sxt a -> Expr (z : a : cxt) sxt b -> Expr cxt sxt t
-}
subst :: HasType (a ': cxt) a -> Expr cxt sxt a -> Expr (a ': cxt) sxt t -> Expr cxt sxt t
subst j s = \case
  -- E.Lam e ->
  --   let
  --     j' = There j -- :: HasType (x : a : cxt) a
  --     s' = shift j s -- :: Expr (x : cxt) sxt a
  --     e' = subst j' s' e -- :: Expr (a : cxt) sxt b
  --   in E.Lam e'
  -- E.App f a -> E.App (subst j s f) (subst j s a)
  -- E.Var Here -> s
  -- E.Var (There j) -> E.Var (There j)

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

shift c = \case
  E.Var i
    -- shift :: HasType cxt t -> Expr cxt sxt t -> Expr cxt sxt t
    -- | i <  c -> E.Var i
    -- shift :: HasType cxt t -> Expr cxt sxt t -> Expr (b : cxt) sxt t
    | i >= c -> E.Var (There i)
  -- E.App f a ->
  --   let
  --     f' = shift c f
  --     a' = shift c a
  --   in E.App _ _
  --
-- shift :: Expr cxt sxt t -> Expr (a ': cxt) sxt t
-- shift = \case
--   -- E.Lam e -> E.Lam (shift e)
--   E.App f a -> E.App (shift f) (shift a)
--   E.Var i -> E.Var (There i)


subst' :: HasType (a ': cxt) a -> Expr cxt sxt a -> Pretask (a ': cxt) sxt b -> Pretask cxt sxt b
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



-- Semantics -------------------------------------------------------------------

{- | Evaluate an expression symbolicaly.

Returns a list of all possible values after evaluation
combined with the predicate which has to hold to get that value.
Note that the context of symbolic values `sxt` is the same for the expression
and the resulting predicate.
-}
eval :: Expr cxt sxt t -> List ( Val cxt sxt t, Pred sxt 'TyBool )
eval = \case
  E.App e1 e2 -> do
    -- e1' :: Expr (a ': cxt ) sxt b
    -- v2  :: Val  cxt sxt a  ~~>  Expr cxt sxt a
    -- sub :: HasType (a ': cxt) a -> Expr cxt sxt a -> Expr (a ': cxt) sxt b -> Expr cxt sxt b
    ( V.Lam e1', p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    ( v1, p3 ) <- eval $ subst Here (asExpr v2) e1'
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
