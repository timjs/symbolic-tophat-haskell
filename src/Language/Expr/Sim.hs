module Language.Expr.Sim where

import Control.Monad.Supply
import Language.Input
import Language.Name
import Language.Type

import Language.Expr (Expr, Pretask)
import Language.Pred (Pred, simplify, pattern Yes, pattern (:/\:))
import Language.Store (MonadStore(..))
import Language.Val (Val, Task, asPred, asExpr)

import qualified Language.Expr as E
import qualified Language.Pred as P
import qualified Language.Store as S
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
  E.Ref a -> E.Ref (subst j s a)
  E.Deref a -> E.Deref (subst j s a)
  E.Assign a b -> E.Assign (subst j s a) (subst j s b)
  E.Task p -> E.Task (subst' j s p)

  E.Loc i -> E.Loc i
  E.Sym i -> E.Sym i
  E.Con p x -> E.Con p x


subst' :: Typeable s => Name s -> Expr s -> Pretask t -> Pretask t
subst' j s = \case
  E.Edit a -> E.Edit (subst j s a)
  E.Enter -> E.Enter
  E.Update a -> E.Update (subst j s a)
  E.And a b -> E.And (subst j s a) (subst j s b)
  E.Or a b -> E.Or (subst j s a) (subst j s b)
  E.Xor a b -> E.Xor (subst j s a) (subst j s b)
  E.Fail -> E.Fail
  E.Then a b -> E.Then (subst j s a) (subst j s b)
  E.Next a b -> E.Next (subst j s a) (subst j s b)


-- | The one-place shift of an expression `e` after cutof `c`.
shift :: Typeable s => Name s -> Expr t -> Expr t
shift c = \case
  E.Lam e -> E.Lam $ shift (c + 1) e
  E.App f a -> E.App (shift c f) (shift c a)
  E.Var i
    | Just Refl <- i ~= c, i >= c -> E.Var (i + 1)
    | otherwise -> E.Var i

  E.Un o a -> E.Un o (shift c a)
  E.Bn o a b -> E.Bn o (shift c a) (shift c b)
  E.If p a b -> E.If (shift c p) (shift c a) (shift c b)
  E.Pair a b -> E.Pair (shift c a) (shift c b)
  E.Fst a -> E.Fst (shift c a)
  E.Snd a -> E.Snd (shift c a)
  E.Ref a -> E.Ref (shift c a)
  E.Deref a -> E.Deref (shift c a)
  E.Assign a b -> E.Assign (shift c a) (shift c b)
  E.Task p -> E.Task (shift' c p)

  E.Loc i -> E.Loc i
  E.Sym i -> E.Sym i
  E.Con p x -> E.Con p x


shift' :: Typeable s => Name s -> Pretask t -> Pretask t
shift' c = \case
  E.Edit a -> E.Edit (shift c a)
  E.Enter -> E.Enter
  E.Update a -> E.Update (shift c a)
  E.And a b -> E.And (shift c a) (shift c b)
  E.Or a b -> E.Or (shift c a) (shift c b)
  E.Xor a b -> E.Xor (shift c a) (shift c b)
  E.Fail -> E.Fail
  E.Then a b -> E.Then (shift c a) (shift c b)
  E.Next a b -> E.Next (shift c a) (shift c b)



-- Observations ----------------------------------------------------------------


value :: MonadStore m => Val ('TyTask t) -> m (Maybe (Val t))
value (V.Task t) = case t of
  V.Edit v1                -> pure $ Just v1
  V.Enter                  -> pure $ Nothing
  V.Update l1              -> map Just $ read l1
  V.And t1 t2              -> do
    mv1 <- value t1
    mv2 <- value t2
    case ( mv1, mv2 ) of
      ( Just v1, Just v2 ) -> pure $ Just $ V.Pair v1 v2
      ( _      , _       ) -> pure $ Nothing
  V.Or t1 t2               -> do
    mv1 <- value t1
    case mv1 of
      Just v1              -> pure $ Just v1
      Nothing              -> do
        mv2 <- value t2
        case mv2 of
          Just v2          -> pure $ Just v2
          Nothing          -> pure $ Nothing
  V.Xor _ _                -> pure $ Nothing
  V.Fail                   -> pure $ Nothing
  V.Then _ _               -> pure $ Nothing
  V.Next _ _               -> pure $ Nothing


failing :: Val ('TyTask t) -> Bool
failing (V.Task t) = case t of
  V.Edit _    -> False
  V.Enter     -> False
  V.Update _  -> False
  V.And t1 t2 -> failing t1 && failing t2
  V.Or  t1 t2 -> failing t1 && failing t2
  V.Xor _ _   -> True --FIXME
  V.Fail      -> True
  V.Then t1 _ -> failing t1
  V.Next t1 _ -> failing t1



-- Semantics -------------------------------------------------------------------

{- | Evaluate an expression symbolicaly.

Returns a list of all possible values after evaluation
combined with the predicate which has to hold to get that value.
Note that the context of symbolic values `sxt` is the same for the expression
and the resulting predicate.
-}
eval
  :: MonadStore m => MonadZero m
  => Expr t -> m ( Val t, Pred 'TyBool )
eval = \case
  E.App e1 e2 -> do
    -- | Apparently GHC can't do GADT pattern matching inside do-notation,
    -- | the compiler complains it needs a `MonadFail` instance.
    -- | Therefore we split out the pattern match in a separate let-binding.
    ( e1', p1 ) <- eval e1
    let V.Lam e11 = e1'
    ( v2, p2 ) <- eval e2
    ( v1, p3 ) <- eval $ subst 0 (asExpr v2) e11
    pure ( v1, p1 :/\: p2 :/\: p3 )

  E.Un o e1 -> do
    ( v1, p1 ) <- eval e1
    pure ( V.Un o v1, p1 )
  E.Bn o e1 e2 -> do
    ( v1, p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    pure ( V.Bn o v1 v2, p1 :/\: p2 )
  E.If e1 e2 e3 ->
    [ ( v2, p1 :/\: p2 :/\: asPred v1 )         | ( v1, p1 ) <- eval e1, ( v2, p2 ) <- eval e2 ] <|>
    [ ( v3, p1 :/\: p3 :/\: P.Not (asPred v1) ) | ( v1, p1 ) <- eval e1, ( v3, p3 ) <- eval e3 ]
  E.Pair e1 e2 -> do
    ( v1, p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    pure ( V.Pair v1 v2, p1 :/\: p2 )
  E.Fst e -> do --FIXME: missing
    ( e', p ) <- eval e
    let V.Pair v _ = e'
    pure ( v, p )
  E.Snd e -> do --FIXME: missing
    ( e', p ) <- eval e
    let  V.Pair _ v = e'
    pure ( v, p )

  E.Ref e1 -> do
    ( v1, p1 ) <- eval e1
    l1 <- S.new v1
    pure ( l1, p1 )
  E.Deref e1 -> do
    ( l1, p1 ) <- eval e1
    v1 <- S.read l1
    pure ( v1, p1 )
  E.Assign e1 e2 -> do
    ( l1, p1 ) <- eval e1
    ( v2, p2 ) <- eval e2
    S.write l1 v2
    pure ( V.U, p1 :/\: p2 )

  E.Lam e ->
    pure ( V.Lam e, Yes )
  E.Loc i ->
    pure ( V.Loc i, Yes )
  E.Sym i ->
    pure ( V.Sym i, Yes )
  E.Con p x ->
    pure ( V.Con p x, Yes )

  E.Task e1 -> do
    ( t1, p1 ) <- eval' e1
    pure ( V.Task t1, p1 )

  E.Var i ->
    error $ "Free variable in expression: " <> show (pretty i)


eval'
  :: MonadStore m => MonadZero m
  => Pretask t -> m ( Task t, Pred 'TyBool )
eval' = \case
  E.Edit e1 -> do
    ( v1, p1 ) <- eval e1
    pure ( V.Edit v1, p1 )
  E.Enter ->
    pure ( V.Enter, Yes )
  E.Update e1 -> do
    ( v1, p1 ) <- eval e1
    pure ( V.Update v1, p1 )
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
    pure ( V.Xor e1 e2, Yes )
  E.Fail ->
    pure ( V.Fail, Yes )
  E.Then e1 e2 -> do
    ( t1, p1 ) <- eval e1
    pure ( V.Then t1 e2, p1 )
  E.Next e1 e2 -> do
    ( t1, p1 ) <- eval e1
    pure ( V.Next t1 e2, p1 )


stride
  :: MonadStore m => MonadZero m
  => Val ('TyTask t) -> m ( Val ('TyTask t), Pred 'TyBool )
stride (V.Task t) = case t of
  -- Step:
  V.Then t1 e2 -> do
    ( t1', p1 ) <- stride t1
    s1 <- inspect
    mv1 <- value t1'
    case mv1 of
      Nothing -> pure ( V.Task $ V.Then t1' e2, p1 )
      Just v1 -> do
        ( t2, p2 ) <- eval $ E.App e2 (asExpr v1)
        if failing t2
          then do
            place s1
            pure ( V.Task $ V.Then t1' e2, p1 )
            -- empty
          else pure ( t2, p1 :/\: p2 )
  -- Choose:
  V.Or t1 t2 -> do
    ( t1', p1 ) <- stride t1
    mv1 <- value t1'
    case mv1 of
      Just _  -> pure ( t1', p1 )
      Nothing -> do
        ( t2', p2 ) <- stride t2
        mv2 <- value t2'
        case mv2 of
          Just _  -> pure ( t2', p1 :/\: p2 )
          Nothing -> pure ( V.Task $ V.Or t1' t2', p1 :/\: p2 )
  -- Evaluate:
  V.And t1 t2 -> do
    ( t1', p1 ) <- stride t1
    ( t2', p2 ) <- stride t2
    pure ( V.Task $ V.And t1' t2', p1 :/\: p2 )
  V.Next t1 e2 -> do
    ( t1', p1 ) <- stride t1
    pure ( V.Task $ V.Next t1' e2, p1 )
  -- Ready:
  t1 ->
    pure ( V.Task t1, Yes )


normalise
  :: MonadStore m => MonadZero m
  => Expr ('TyTask t) -> m ( Val ('TyTask t), Pred 'TyBool )
normalise e0 = do
  ( t0, p0 ) <- eval e0
  s1 <- inspect
  ( t1, p1 ) <- stride t0
  s2 <- inspect
  if t0 == t1 && s1 == s2
    then pure ( t1, p0 :/\: p1 )
    else do
      ( t2, p2 ) <- normalise $ asExpr t1
      pure ( t2, p0 :/\: p1 :/\: p2 )
  -- pure ( t1, p0 :/\: p1 )


initialise
  :: MonadStore m => MonadZero m
  => Expr ('TyTask t) -> m ( Val ('TyTask t), Pred 'TyBool )
initialise = normalise


handle
  :: MonadSupply Nat m => MonadStore m => MonadZero m
  => Val ('TyTask t) -> m ( Val ('TyTask t), Input, Pred 'TyBool )
handle (V.Task t) = case t of
  V.Edit _ -> do
    s <- fresh
    pure ( V.Task $ V.Edit (V.Sym s), Change s, Yes )
  V.Enter -> do
    s <- fresh
    pure ( V.Task $ V.Edit (V.Sym s), Change s, Yes )
  V.Update l -> do
    s <- fresh
    write l (V.Sym s)
    pure ( V.Task $ V.Update l, Change s, Yes )
  V.And t1 t2 ->
    [ ( V.Task $ V.And t1' t2, ToFirst  i1, p1 ) | ( t1', i1, p1 ) <- handle t1 ] <|>
    [ ( V.Task $ V.And t1 t2', ToSecond i2, p2 ) | ( t2', i2, p2 ) <- handle t2 ]
  V.Or t1 t2 ->
    [ ( V.Task $ V.Or t1' t2, ToFirst  i1, p1 ) | ( t1', i1, p1 ) <- handle t1 ] <|>
    [ ( V.Task $ V.Or t1 t2', ToSecond i2, p2 ) | ( t2', i2, p2 ) <- handle t2 ]
  V.Xor e1 e2 ->
    [ ( t1, Change s, p1 :/\: P.Sym s )         | ( t1, p1 ) <- normalise e1, s <- fresh, not (failing t1) ] <|>
    [ ( t2, Change s, p2 :/\: P.Not (P.Sym s) ) | ( t2, p2 ) <- normalise e2, s <- fresh, not (failing t2) ]
  V.Fail -> empty
    -- NOTE: Alternative: users can input anything, but nothing will ever come out of `fail`
    -- s <- fresh
    -- pure ( V.Task $ V.Fail, Change s, P.Nop )
  V.Then t1 e2 -> do
    ( t1', i1, p1 ) <- handle t1
    pure ( V.Task $ V.Then t1' e2, i1, p1 )
  V.Next t1 e2 ->
    [ ( V.Task $ V.Next t1' e2, i1, p1 ) | ( t1', i1, p1 ) <- handle t1 ] <|>
    [ ( t2, Continue, p2 )               | Just v1 <- value t1, ( t2, p2 ) <- normalise (E.App e2 (asExpr v1)), not (failing t2) ]


newtype Info t = Info ( Val ('TyTask t), Input, Pred 'TyBool )


drive
  :: MonadTrace m => MonadSupply Nat m => MonadStore m => MonadZero m
  => Val ('TyTask t) -> m ( Val ('TyTask t), Input, Pred 'TyBool )
drive t0 = do
  ( t1, i1, p1 ) <- handle t0
  trace ( i1, i1, t1, p1 )
  ( t2, p2 ) <- normalise (asExpr t1)
  trace ( t2, p1 )
  pure ( t2, i1, p1 :/\: p2 )


-- | Call `drive` till the moment we have an observable value.
-- | Collects all inputs and predicates created in the mean time.
simulate
  :: MonadTrace m => MonadSupply Nat m => MonadStore m => MonadZero m
  => Val ('TyTask t) -> List Input -> Pred 'TyBool -> m ( Val ('TyTask t), List Input, Pred 'TyBool )
simulate t is p = go (go end) t is p
  where
    go cont t0 is0 ps0 = do
      ( t1, i1, p1 ) <- drive t0
      mv1 <- value t1
      let ps1 = ps0 :/\: p1
      let is1 = i1 : is0
      if| not (satisfiable ps1) -> empty
        | Just _ <- mv1         -> pure ( t1, reverse is1, simplify ps1 )
        | t0 /= t1              -> simulate t1 is1 ps1
        | otherwise             -> cont t1 is1 ps1
    end _ _ _ = empty


satisfiable :: Pred 'TyBool -> Bool
satisfiable _ = True  -- FIXME: use SBV here


run
  :: MonadTrace m => MonadSupply Nat m => MonadStore m => MonadZero m
  => Expr ('TyTask t) -> m ( Val ('TyTask t), List Input, Pred 'TyBool )
run t0 = do
  ( t1, p1 ) <- initialise t0
  simulate t1 empty p1
