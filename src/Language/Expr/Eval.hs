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


eval :: Env cxt -> Env sxt -> Expr cxt sxt t -> TypeOf t
eval vars syms = \case
  Lam f -> \x -> eval (Cons x vars) syms f
  App f a -> eval vars syms f $ eval vars syms a
  Var i -> lookup i vars
  Val i -> i

  Un o a -> (un o) (eval vars syms a)
  Bin o a b -> (bin o) (eval vars syms a) (eval vars syms b)
  If p a b -> if eval vars syms p then eval vars syms a else eval vars syms b

  Unit -> ()
  Pair a b -> ( eval vars syms a, eval vars syms b )


eval' :: Expr '[] '[] t -> TypeOf t
eval' = eval Nil Nil
