module Language.Expr.Eval where


import Language.Expr



-- Environments ----------------------------------------------------------------


data Env (cxt :: List u) where
  Nil :: Env '[]
  Cons :: TypeOf t -> Env ts -> Env (t ': ts)


lookup :: HasType cxt t -> Env cxt -> TypeOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs



-- Evaluation ------------------------------------------------------------------


un :: Un a b -> TypeOf a -> TypeOf b
un = \case
    Not -> not

    Neg -> negate

    Fst -> fst
    Snd -> snd


bn :: Bn a b c -> TypeOf a -> TypeOf b -> TypeOf c
bn = \case
  And -> (&&)
  Or  -> (||)

  Lt -> (<)
  Le -> (<=)
  Eq -> (==)
  Nq -> (/=)
  Ge -> (>=)
  Gt -> (>)

  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div


eval :: Env cxt -> Expr cxt sxt t -> Env sxt -> TypeOf t
eval vars = \case
  Lam f -> \syms x -> eval (Cons x vars) f syms
  App f a -> eval vars f <*> eval vars a
  Var i -> pure $ lookup i vars
  Sym i -> lookup i
  Val i -> pure i

  Un o a -> un o <$> eval vars a
  Bn o a b -> bn o <$> eval vars a <*> eval vars b
  If p a b -> bool <$> eval vars a <*> eval vars b <*> eval vars p

  Unit -> pure ()
  Pair a b -> eval vars a <&> eval vars b


eval' :: Expr '[] sxt t -> Env sxt -> TypeOf t
eval' = eval Nil
