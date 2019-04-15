module Language.Expr.Simulate where

import Language.Names
import Language.Types

import Language.Expr (Expr, Pretask)
import Language.Pred (Pred, pattern Yes, pattern (:/\:))
import Language.Val (Val, Task, asPred, asExpr)

import qualified Language.Expr as E
import qualified Language.Pred as P
import qualified Language.Val as V


--------------------------------------------------------------------------------

-- Substitution --


subst
  :: Typeable s
  => Name s  -- ^ Substitute a name of type `s`
  -> Expr s  -- ^ with expression of type `s`
  -> Expr t  -- ^ in an expression of type `t` containing a variable of type `a`
  -> Expr t  -- ^ giving the modified expression of type `t`.
subst j s = \case
  E.Lam e -> go e
    where
      go :: forall a b. Typeable a => Expr b -> Expr (a ':-> b)
      go = E.Lam << subst (j + 1) (shift (0 :: Name a) s)  -- We need to tell Haskell that `0` is of type `Name a` here
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
  E.Edit a -> E.Edit (subst j s a)
  E.Enter -> E.Enter
  -- Store ->
  E.And a b -> E.And (subst j s a) (subst j s b)
  E.Or a b -> E.Or (subst j s a) (subst j s b)
  E.Xor a b -> E.Xor (subst j s a) (subst j s b)
  E.Fail -> E.Fail
  E.Then a b -> E.Then (subst j s a) (subst j s b)
  E.Next a b -> E.Next (subst j s a) (subst j s b)


-- | The one-place shift of an expression `e` after cutof `c`.
shift :: Typeable a => Name a -> Expr b -> Expr b
shift c = \case
  E.Lam e -> E.Lam $ shift (c + 1) e
  E.App f a -> E.App (shift c f) (shift c a)
  E.Var i
    | Just Refl <- i ~= c
    , i >= c -> E.Var (i + 1)
    | otherwise -> E.Var i

  E.Un o a -> E.Un o (shift c a)
  E.Bn o a b -> E.Bn o (shift c a) (shift c b)
  E.If p a b -> E.If (shift c p) (shift c a) (shift c b)
  E.Pair a b -> E.Pair (shift c a) (shift c b)
  E.Fst a -> E.Fst (shift c a)
  E.Snd a -> E.Snd (shift c a)
  E.Task p -> E.Task (shift' c p)

  E.Sym i -> E.Sym i
  E.Con p x -> E.Con p x
  E.Unit -> E.Unit


shift' :: Typeable a => Name a -> Pretask b -> Pretask b
shift' c = \case
  E.Edit a -> E.Edit (shift c a)
  E.Enter -> E.Enter
  -- Store ->
  E.And a b -> E.And (shift c a) (shift c b)
  E.Or a b -> E.Or (shift c a) (shift c b)
  E.Xor a b -> E.Xor (shift c a) (shift c b)
  E.Fail -> E.Fail
  E.Then a b -> E.Then (shift c a) (shift c b)
  E.Next a b -> E.Next (shift c a) (shift c b)



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

  E.Lam e ->
    [ ( V.Lam e, Yes ) ]
  E.Sym i ->
    [ ( V.Sym i, Yes ) ]
  E.Con p x ->
    [ ( V.Con p x, Yes ) ]
  E.Unit ->
    [ ( V.Unit, Yes )]

  E.Task e1 -> do
    ( t1, p1 ) <- eval' e1
    [ ( V.Task t1, p1 ) ]

  E.Var i ->
    error $ "Free variable in expression: " <> show (pretty i)


eval' :: Pretask t -> List ( Task t, Pred 'TyBool )
eval' = \case
  E.Edit e1 -> do
    ( v1, p1 ) <- eval e1
    pure ( V.Edit v1, p1 )
  E.Enter ->
    [ ( V.Enter, Yes ) ]
  -- E.Store -> do
  E.And e1 e2 -> do
    ( t1, p1 ) <- eval e1
    ( t2, p2 ) <- eval e2
    pure ( V.And t1 t2, p1 :/\: p2 )
  E.Or e1 e2 -> do
    ( t1, p1 ) <- eval e1
    ( t2, p2 ) <- eval e2
    pure ( V.Or t1 t2, p1 :/\: p2 )
  E.Xor e1 e2 ->
    -- | Here we do not need to evaluate because `Xor` is lazy.
    [ ( V.Xor e1 e2, Yes ) ]
  E.Fail ->
    [ ( V.Fail, Yes ) ]
  E.Then e1 e2 -> do
    ( t1, p1 ) <- eval e1
    pure ( V.Then t1 e2, p1 )
  E.Next e1 e2 -> do
    ( t1, p1 ) <- eval e1
    pure ( V.Next t1 e2, p1 )
