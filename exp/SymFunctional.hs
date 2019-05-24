module Tophat.Expr.SymFunctional where


import Tophat.Expr

import Data.SBV



-- Types -----------------------------------------------------------------------

type family SymOf (a :: Ty) where
  SymOf (a ':-> b) = SymOf a -> SymOf b
  SymOf (a ':>< b) = ( SymOf a, SymOf b )

  SymOf 'TyUnit = SBV ()
  SymOf 'TyBool = SBV Bool
  SymOf 'TyInt = SBV Integer
  SymOf 'TyString = SBV String



-- Environment -----------------------------------------------------------------


data Env (cxt :: List u) where
  Nil :: Env '[]
  Cons :: SymOf t -> Env ts -> Env (t ': ts)


lookup :: HasType cxt t -> Env cxt -> SymOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs



-- Symbolic --------------------------------------------------------------------


un :: Un a b -> SymOf a -> SymOf b
un = \case
    Not -> sNot

    Neg -> negate

    Fst -> fst
    Snd -> snd


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


eval :: Env cxt -> Expr cxt sxt t -> Env sxt -> SymOf t
eval vars = \case
  Lam f -> \syms x -> eval (Cons x vars) f syms
  App f a -> eval vars f <*> eval vars a
  Var i -> pure $ lookup i vars
  Sym i -> lookup i
  Val i -> pure $ literal i -- FIXME: needs proof that `i` is a basic type and is convertible to an SBV value

  Un o a -> un o <$> eval vars a
  Bn o a b -> bn o <$> eval vars a <*> eval vars b
  If p a b -> ite <$> eval vars p <*> eval vars a <*> eval vars b

  Unit -> pure $ literal ()
  Pair a b -> eval vars a <&> eval vars b


eval' :: Expr '[] sxt t -> Env sxt -> SymOf t
eval' = eval Nil
