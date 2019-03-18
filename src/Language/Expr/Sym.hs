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


{- Gathering ------------------------------------------------------------------


gather :: SymbEnv cxt -> Expr cxt sxt t -> List (Writer (List (Pred sxt 'TyBool)) (Expr cxt sxt t))
gather vars = \case

  -- Lam f -> \x -> gather (Cons x vars) f
  -- App f a -> gather vars f $ gather vars a
  -- Var i -> lookup i vars
  -- Sym _ -> error "Found Sym in executable expression" --FIXME: should be checkable
  -- Val i -> i

  Un o a -> lift1 (Un o) <$> gather vars a
  Bn o a b -> lift2 (Bn o) <$> gather vars a <*> gather vars b
  If p a b -> do
    p' <- gather vars p
    ( p'', _ ) <- runWriter p'
    _
    -- if gather vars p then gather vars a else gather vars b

  Unit -> pure $ pure Unit
  Pair a b -> lift2 Pair <$> gather vars a <*> gather vars b

  Fst e -> lift1 Fst <$> gather vars e
  Snd e -> lift1 Snd <$> gather vars e

-}


{- Gathering ------------------------------------------------------------------


gather :: SymbEnv cxt -> Expr cxt sxt t -> List (Writer (List (Pred sxt 'TyBool)) (Expr cxt sxt t))
gather vars = \case

  -- Lam f -> \x -> gather (Cons x vars) f
  -- App f a -> gather vars f $ gather vars a
  -- Var i -> lookup i vars
  -- Sym _ -> error "Found Sym in executable expression" --FIXME: should be checkable
  -- Val i -> i

  Un o a -> lift1 (Un o) <$> gather vars a
  Bn o a b -> lift2 (Bn o) <$> gather vars a <*> gather vars b
  If p a b -> do
    p' <- gather vars p
    ( p'', _ ) <- runWriter p'
    _
    -- if gather vars p then gather vars a else gather vars b

  Unit -> pure $ pure Unit
  Pair a b -> lift2 Pair <$> gather vars a <*> gather vars b

  Fst e -> lift1 Fst <$> gather vars e
  Snd e -> lift1 Snd <$> gather vars e

--}
