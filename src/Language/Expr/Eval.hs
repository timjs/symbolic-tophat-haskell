module Language.Expr.Eval where


import Language.Expr



-- Evaluation ------------------------------------------------------------------


un :: Un a b -> TypeOf a -> TypeOf b
un = \case
    Not -> not

    Neg -> negate


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


eval :: Env cxt -> Expr cxt '[] t -> TypeOf t
eval vars = \case
  Lam f -> \x -> eval (Cons x vars) f
  App f a -> eval vars f $ eval vars a
  Var i -> lookup i vars
  Sym _ -> error "Found Sym in executable expression" --FIXME: should be checkable
  Val i -> i

  Un o a -> un o (eval vars a)
  Bn o a b -> bn o (eval vars a) (eval vars b)
  If p a b -> if eval vars p then eval vars a else eval vars b

  Unit -> ()
  Pair a b -> ( eval vars a, eval vars b )
  Fst e -> fst $ eval vars e
  Snd e -> snd $ eval vars e


eval' :: Expr '[] '[] t -> TypeOf t
eval' = eval Nil
