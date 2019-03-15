module Language.Expr.SymDynamic where


import Language.Expr

import Data.SBV.Dynamic



-- Environment -----------------------------------------------------------------


data Env (cxt :: List u) where
  Nil :: Env '[]
  Cons :: SymOf t -> Env ts -> Env (t ': ts)


lookup :: HasType cxt t -> Env cxt -> SymOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs



-- Universe --------------------------------------------------------------------


type family SymOf (a :: Ty) where
  SymOf (a ':-> b) = SVal -> SVal
  SymOf a = SVal



-- Symbolic --------------------------------------------------------------------


un :: Un a b -> SymOf a -> SymOf b
un = \case
    Not -> svNot

    Neg -> svUNeg

    -- Fst -> fst
    -- Snd -> snd


bn :: Bn a b c -> SymOf a -> SymOf b -> SymOf c
bn = \case
  And -> svAnd
  Or  -> svOr

  Lt -> svLessThan
  Le -> svLessEq
  Eq -> svEqual
  Nq -> svNotEqual
  Ge -> svGreaterEq
  Gt -> svGreaterThan

  Add -> svPlus
  Sub -> svMinus
  Mul -> svTimes
  Div -> svDivide --TODO: correct one?


eval :: Env cxt -> Expr cxt '[] t -> SymOf t
eval vars = \case
  Lam f -> \x -> eval (Cons x vars) f
  App f a -> eval vars f $ eval vars a
  Var i -> lookup i vars
  Sym _ -> error "Found Sym in executable expression" --FIXME: should be checkable
  Val i -> i

  Un o a -> un o (eval vars a)
  Bn o a b -> bn o (eval vars a) (eval vars b)
  If p a b -> svIte (eval vars p) (eval vars a) (eval vars b)

  Unit -> ()
  Pair a b -> ( eval vars a, eval vars b )



-- sym :: SymEnv cxt -> Expr cxt sxt t -> SymOf t
-- sym env = \case
--   Lam f -> \x -> sym (Cons x env) f
--   App f a -> sym env f $ sym env a
  -- Var i -> lookup i env
  -- Val i -> i
--
--   Un o a -> (un o) (sym env a)
--   Bn o a b -> (bn o) (sym env a) (sym env b)
--   -- If p a b -> ite (sym env p) (sym env a) (sym env b)
--
--   Unit -> ()
--   Pair a b -> ( sym env a, sym env b )
