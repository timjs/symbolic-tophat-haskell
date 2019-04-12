module Language.Expr.Eval where

{-
import qualified Data.Task as Task

import Language.Expr



-- Evaluation ------------------------------------------------------------------


un :: Un a b -> ConcOf a -> ConcOf b
un = \case
  Not -> not
  Neg -> negate


bn :: Bn a b c -> ConcOf a -> ConcOf b -> ConcOf c
bn = \case
  Conj -> (&&)
  Disj -> (||)

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
eval :: ConcEnv cxt -> Expr t -> ConcOf t
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

  Task x -> preval vars x


preval :: ConcEnv cxt -> Pretask t -> ConcOf t
preval vars = \case
  Edit x -> Task.Edit $ Just $ eval vars x
  Enter -> Task.Edit Nothing
  -- Store -> _
  And x y -> Task.And (eval vars x) (eval vars y)
  Or x y -> Task.Or (eval vars x) (eval vars y)
  Xor x y -> Task.Xor (eval vars x) (eval vars y)
  Fail -> Task.Fail
  Then t c -> Task.Then (eval vars t) (eval vars c)
  Next t c -> Task.Next (eval vars t) (eval vars c)


eval' :: Expr t -> ConcOf t
eval' = eval Nil

-}
