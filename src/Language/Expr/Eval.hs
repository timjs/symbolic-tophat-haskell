module Language.Expr.Eval where


import Language.Expr



-- Evaluation ------------------------------------------------------------------


un :: Un a b -> ConcOf a -> ConcOf b
un = \case
  Not -> not
  Neg -> negate


bn :: Bn a b c -> ConcOf a -> ConcOf b -> ConcOf c
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


-- | Evaluation of expressions
-- |
-- | Evaluates any expression to its Haskell equivallent.
-- | Note that this expression (statically!) cannot contain any symbolic variables!
eval :: ConcEnv cxt -> Expr cxt '[] t -> ConcOf t
eval vars = \case
  Lam f -> \x -> eval (Cons x vars) f
  App f a -> eval vars f $ eval vars a
  Var i -> lookup i vars

  Con BoolIsPrim x -> x
  Con IntIsPrim x -> x
  Con StringIsPrim x -> x

  Un o a -> un o (eval vars a)
  Bn o a b -> bn o (eval vars a) (eval vars b)
  If p a b -> if eval vars p then eval vars a else eval vars b

  Unit -> ()
  Pair a b -> ( eval vars a, eval vars b )
  Fst e -> fst $ eval vars e
  Snd e -> snd $ eval vars e


eval' :: Expr '[] '[] t -> ConcOf t
eval' = eval Nil
