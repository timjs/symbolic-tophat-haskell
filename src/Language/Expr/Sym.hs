module Language.Expr.Sym where


import Language.Expr

import Data.SBV



-- Environment -----------------------------------------------------------------


data SymEnv (cxt :: List Ty) where
  Nil :: SymEnv '[]
  Cons :: SymOf t -> SymEnv ts -> SymEnv (t ': ts)


lookup :: HasType cxt t -> SymEnv cxt -> SymOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs



-- Symbolic --------------------------------------------------------------------


un :: Un a b -> SymOf a -> SymOf b
un = \case
    Not -> sNot

    Fst -> fst
    Snd -> snd


bin :: Bin a b c -> SymOf a -> SymOf b -> SymOf c
bin = \case
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


sym :: SymEnv cxt -> Expr cxt t -> SymOf t
sym env = \case
  Lam f -> \x -> sym (Cons x env) f
  App f a -> sym env f $ sym env a
  Var i -> lookup i env
  -- Val i -> i

  Un o a -> (un o) (sym env a)
  Bin o a b -> (bin o) (sym env a) (sym env b)
  -- If p a b -> ite (sym env p) (sym env a) (sym env b)

  Unit -> ()
  Pair a b -> ( sym env a, sym env b )
