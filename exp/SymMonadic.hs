module Tophat.Expr.SymMonadic where

import Tophat.Expr

import Data.SBV



-- Environment -----------------------------------------------------------------


data Env (cxt :: List u) where
  Nil :: Env '[]
  Cons :: SymOf t -> Env ts -> Env (t ': ts)


lookup :: HasType cxt t -> Env cxt -> SymOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs



-- Universe --------------------------------------------------------------------


type family SymOf (a :: Ty) where
  SymOf (a ':-> b) = SymOf a -> Symbolic (SymOf b)
  SymOf (a ':>< b) = ( SymOf a, SymOf b )

  SymOf 'TyUnit = SBV ()
  SymOf 'TyBool = SBV Bool
  SymOf 'TyInt = SBV Integer
  SymOf 'TyString = SBV String



-- Symbolic --------------------------------------------------------------------


un :: Un a b -> SymOf a -> SymOf b
un = \case
    Not -> sNot

    Neg -> negate

    -- Fst -> fst
    -- Snd -> snd


bn :: Bn a b c -> SymOf a -> SymOf b -> SymOf c
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


eval :: Env cxt -> Expr cxt sxt t -> Symbolic (SymOf t)
eval vars = \case
  Lam f -> pure \x -> eval (Cons x vars) f
  App f a -> do
    f' <- eval vars f
    a' <- eval vars a
    f' a'
  Var i -> pure $ lookup i vars
  Sym i -> free $ show $ idx i
  Val i -> pure $ literal i

  Un o a -> un o <$> eval vars a
  Bn o a b -> bn o <$> eval vars a <*> eval vars b
  If p a b -> ite <$> eval vars p <*> eval vars a <*> eval vars b

  Unit -> pure $ literal ()
  Pair a b -> eval vars a <&> eval vars b


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
