module Test.Exprs where

import Data.Stream (Stream)
import Language.Input (Input)
import Language.Pred (Pred, pattern (:/\:))
import Language.Val (Val, asExpr)

import Control.Monad.List
import Control.Monad.Supply
import Language.Expr
import Language.Expr.Sim

import qualified Data.Stream as Stream


-- Examples --------------------------------------------------------------------

-- Functions --


double_mul :: Expr ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
double_mul = Lam (Bn Mul (I 2) (Var 0))


double_add :: Expr ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
double_add = Lam (Bn Add (Var 0) (Var 0))


abs :: Expr ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
abs = Lam
  (If (Bn Lt (Var 0) (I 0))
    (Un Neg (Var 0))
    (Var 0))


add :: Expr ('TyPrim 'TyInt ':-> ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt))
add = Lam (Lam (
  Bn Add (Var 0) (Var 1)))


fact :: Expr ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
fact = Lam
  (If (Bn Eq (Var 0) (I 0))
    (I 1)
    (Bn Mul (App fact (Bn Sub (Var 0) (I 1))) (Var 0)))



-- Tasks --

echo :: Pretask ('TyTask ('TyPrim 'TyInt))
echo =
  Enter @'TyInt :>>=
  View (Var 0)


add_seq :: Pretask ('TyTask ('TyPrim 'TyInt))
add_seq =
  Enter @'TyInt :>>=
  Enter @'TyInt :>>=
  View (Bn Add (Var 0) (Var 1))


type TyIntInt = 'TyPrim 'TyInt ':>< 'TyPrim 'TyInt

add_par :: Pretask ('TyTask ('TyPrim 'TyInt))
add_par =
  Enter @'TyInt :&&: Enter @'TyInt :>>=
  View (Bn Add (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0)))


guard :: Pretask ('TyTask ('TyPrim 'TyInt))
guard =
  Task (Enter @'TyInt) `Then` Lam (
    If (Bn Gt (Var @('TyPrim 'TyInt) 0) (I 0)) (Task $ View (Var @('TyPrim 'TyInt) 0)) (Task $ Fail)
  )



-- Main ------------------------------------------------------------------------


type Runner = ListT (Supply Int)
  -- == Stream Int -> ( List a, Stream Int )

ids :: Stream Int
ids = Stream.iterate succ 0

run :: Runner a -> List a
run r = fst $ runSupply (runListT r) ids


test :: Pretask ('TyTask t) -> List ( Val ('TyTask t), List Input, Pred 'TyBool )
test t0 = run do
  ( t1, p1 ) <- initialise (Task t0)
  simulate t1 empty p1

-- main = do
--   print $ pretty $ test echo
