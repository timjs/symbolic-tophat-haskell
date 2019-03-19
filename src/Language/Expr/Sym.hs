module Language.Expr.Sym where


import Data.SBV

import Language.Expr



-- Symbolic --------------------------------------------------------------------


un :: Un a b -> SymbOf a -> SymbOf b
un = \case
  Not -> sNot
  Neg -> negate


bn :: Bn a b c -> SymbOf a -> SymbOf b -> SymbOf c
bn = \case
  And -> (.&&)
  Or  -> (.||)

  Lt -> (.<)
  Le -> (.<=)
  Eq -> (.==)
  Nq -> (./=)
  Ge -> (.>=)
  Gt -> (.>)

  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> sDiv


eval :: SymbEnv cxt -> Expr cxt sxt t -> SymbEnv sxt -> SymbOf t
eval vars = \case
  Lam f -> \syms x -> eval (Cons x vars) f syms
  App f a -> eval vars f <*> eval vars a
  Var i -> pure $ lookup i vars

  Sym i -> lookup i
  Con BoolIsPrim x -> pure $ literal x
  Con IntIsPrim x -> pure $ literal x
  Con StringIsPrim x -> pure $ literal x

  Un o a -> un o <$> eval vars a
  Bn o a b -> bn o <$> eval vars a <*> eval vars b
  If p a b -> ite <$> eval vars p <*> eval vars a <*> eval vars b

  Unit -> pure ()
  Pair a b -> eval vars a <&> eval vars b
  Fst e -> fst <$> eval vars e
  Snd e -> snd <$> eval vars e


eval' :: Expr '[] sxt t -> SymbEnv sxt -> SymbOf t
eval' = eval Nil


eval'' :: Expr '[] '[] t -> SymbOf t
eval'' e = eval Nil e Nil



{- Gathering ------------------------------------------------------------------


--FIXME: Use something other than List for optimisation
gather :: SymbEnv cxt -> Expr cxt sxt t -> Writer (List (Pred sxt 'TyBool)) (Expr cxt sxt t)
gather vars = \case

  Lam f -> _ -- \x -> gather (Cons x vars) f
  App f a -> App <$> gather vars f <*> gather vars a
  Var i -> pure $ Var i
  Sym i -> _

  Con p x -> _

  Un o a -> Un o <$> gather vars a
  Bn o a b -> Bn o <$> gather vars a <*> gather vars b
  If p a b -> do
    p' <- gather vars p
    tell [ toPred p' ]
    case p' of
      B True -> gather vars a
      B False -> gather vars b
    -- let ( p'', w ) = runWriter p'
    --
    -- if gather vars p then gather vars a else gather vars b

  Unit -> pure Unit
  Pair a b -> Pair <$> gather vars a <*> gather vars b

  Fst e -> (\(Pair a _) -> a) <$> gather vars e
  Snd e -> (\(Pair _ b) -> b) <$> gather vars e


--FIXME: Should `cxt` be empty?
toPred :: Expr cxt sxt t -> Pred sxt t
toPred = \case
  Lam _ -> error "left over lambda"
  App _ _ -> error "left over application"
  Var _ -> error "left over variable"
  Sym i -> Pred.Sym i

  Con p x -> Pred.Con p x

  Un u a -> Pred.Un u (toPred a)
  Bn o a b -> Pred.Bn o (toPred a) (toPred b)
  If _ _ _ -> error "left over if-then-else"

  Unit -> error "left over unit"
  Pair _ _ -> error "left over pair"
  Fst _ -> error "left over fst"
  Snd _ -> error "left over snd"
-}

{-
gather :: SymbEnv cxt -> Expr cxt sxt t -> List (Writer (List (Pred sxt 'TyBool)) (Expr cxt sxt t))
gather vars = \case

  -- Lam f -> \x -> gather (Cons x vars) f
  App f a -> lift2 App <$> gather vars f <*> gather vars a
  Var i -> pure $ pure $ lookup i vars
  -- Val i -> i

  Un o a -> lift1 (Un o) <$> gather vars a
  Bn o a b -> lift2 (Bn o) <$> gather vars a <*> gather vars b
  If p a b -> do
    p' <- gather vars p
    let ( p'', w ) = runWriter p'
    case p'' of
      B True -> _
      B False -> do

    -- if gather vars p then gather vars a else gather vars b

  Unit -> pure $ pure Unit
  Pair a b -> lift2 Pair <$> gather vars a <*> gather vars b

  Fst e -> lift1 (\(Pair a b) -> a) <$> gather vars e
  Snd e -> lift1 (\(Pair a b) -> b) <$> gather vars e
-}
