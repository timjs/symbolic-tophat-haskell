module Language.Expr.Eval where


import Language.Expr



-- Environment -----------------------------------------------------------------


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


eval :: Env cxt -> Expr cxt t -> TypeOf t
eval env = \case
  Lam f -> \x -> eval (Cons x env) f
  App f a -> eval env f $ eval env a
  Var i -> lookup i env
  Val i -> i

  Un o a -> (un o) (eval env a)
  Bin o a b -> (bin o) (eval env a) (eval env b)
  If p a b -> if eval env p then eval env a else eval env b

  Unit -> ()
  Pair a b -> ( eval env a, eval env b )


eval' :: Expr '[] t -> TypeOf t
eval' = eval Nil
