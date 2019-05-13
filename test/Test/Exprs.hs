module Test.Exprs where

import Data.Stream (Stream)
import Language.Input (Input)
import Language.Pred (Pred, pattern (:/\:))
import Language.Val (Val, asExpr)

import Control.Monad.List
import Control.Monad.Supply
import Control.Monad.Trace
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


guard1 :: Pretask ('TyTask ('TyPrim 'TyInt))
guard1 =
  Enter @'TyInt :>>\
  If (Bn Gt (Var @('TyPrim 'TyInt) 0) (I 0)) (Task $ View (Var @('TyPrim 'TyInt) 0)) (Task $ Fail)


guard2 :: Pretask ('TyTask ('TyPrim 'TyInt))
guard2 =
  Edit (Sym @'TyInt -1) :>>\
  If (Bn Gt (Var @('TyPrim 'TyInt) 0) (I 0)) (Task $ View (Var @('TyPrim 'TyInt) 0)) (Task $ Fail)



-- Main ------------------------------------------------------------------------


type Runner = ListT (SupplyT Int Tracer)
  -- == Stream Int -> ( ( List a, Stream Int ), List (Doc n) )

ids :: Stream Int
ids = Stream.iterate succ 0

trace :: Runner a -> List (Doc ())
trace r = snd $ runTracer (runSupplyT (runListT r) ids)

exec :: Runner a -> List a
exec r = fst $ fst $ runTracer (runSupplyT (runListT r) ids)


{-
>>> print $ trace $ run echo
[(, ⊠ ▶ λ.□(x0), (True ∧ True))
,(s0, □(s0) ▶ λ.□(x0), True)
,(, □(s0), (True ∧ (True ∧ ((True ∧ True) ∧ True))))
]

>>> print $ trace $ run add_seq
[,(  , ⊠ ▶ λ.⊠ ▶ λ.□((x0 + x1))    , (True ∧ True))
 ,(s0, □(s0) ▶ λ.⊠ ▶ λ.□((x0 + x1)), True)
 ,(  , ⊠ ▶ λ.□((x0 + s0))          , (True ∧ (True ∧ ((True ∧ True) ∧ True))))
 ,(s1, □(s1) ▶ λ.□((x0 + s0))      , True)
 ,(  , □((s1 + s0))                , (True ∧ (True ∧ ((True ∧ True) ∧ (True ∧ True)))))]

>>> print $ trace $ run add_par
[,(    , ⊠ ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))        , ((True ∧ True) ∧ (True ∧ True)))
 ,(F s0, □(s0) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , □(s0) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(S s1, ⊠ ⋈ □(s1) ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , ⊠ ⋈ □(s1) ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(F s2, □(s2) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , □(s2) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(S s3, □(s0) ⋈ □(s3) ▶ λ.□((fst x0 + snd x0)), True)
 ,(    , □((s0 + s3))                          , ((True ∧ True) ∧ ((True ∧ True) ∧ ((True ∧ (True ∧ True)) ∧ ((True ∧ True) ∧ (True ∧ True))))) )
 ,(F s4, □(s4) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , □(s4) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(S s5, □(s2) ⋈ □(s5) ▶ λ.□((fst x0 + snd x0)), True)
 ,(    , □((s2 + s5))                          , ((True ∧ True) ∧ ((True ∧ True) ∧ ((True ∧ (True ∧ True)) ∧ ((True ∧ True) ∧ (True ∧ True))))) )
 ,(F s6, □(s6) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , True),(, □(s6) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0)), ((True ∧ True) ∧ (True ∧ True))),(S s7, □(s4) ⋈ □(s7) ▶ λ.□((fst x0 + snd x0)), True),(
...
]
-}
