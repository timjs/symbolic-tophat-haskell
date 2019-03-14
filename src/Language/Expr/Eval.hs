module Language.Expr.Eval where


import Data.SBV

import Language.Expr



-- Environments ----------------------------------------------------------------


data Env (cxt :: List Ty) where
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


bin :: Bin a b c -> TypeOf a -> TypeOf b -> TypeOf c
bin = \case
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


eval :: Env cxt -> Expr cxt sxt t -> Symbolic (TypeOf t)
eval vars = \case
  Lam f -> pure \x -> eval (Cons x vars) f
  App f a -> do
    f' <- eval vars f
    a' <- eval vars a
    f' a'
  Var i -> pure $ lookup i vars
  -- Sym i -> symbolic (show $ idx i)
  Val i -> pure i

  Un o a -> un o <$> eval vars a
  Bin o a b -> bin o <$> eval vars a <*> eval vars b
  If p a b -> bool <$> eval vars a <*> eval vars b <*> eval vars p

  Unit -> pure ()
  Pair a b -> eval vars a <&> eval vars b


eval' :: Expr '[] '[] t -> Symbolic (TypeOf t)
eval' = eval Nil
