module Test.Exprs where

import Data.Stream (Stream)

import Control.Monad.List
import Control.Monad.Supply
import Control.Monad.Trace
import Language.Expr

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


echo' :: Pretask ('TyTask ('TyPrim 'TyInt))
echo' =
  Enter @'TyInt :>>?
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
  Enter @'TyInt :>>\
  If (Bn Gt (Var @('TyPrim 'TyInt) 0) (I 0)) (Task $ View (Var @('TyPrim 'TyInt) 0)) (Task $ Fail)


preguard :: Pretask ('TyTask ('TyPrim 'TyString))
preguard =
  Enter @'TyBool :>>\
  If (Un Not (Var @('TyPrim 'TyBool) 0)) (Task $ guard2 (Var @('TyPrim 'TyBool) 0)) (Task $ Fail)

guard2 :: Expr ('TyPrim 'TyBool) -> Pretask ('TyTask ('TyPrim 'TyString))
guard2 x =
  Task (Edit x) `Next` Lam (
    If (Var @('TyPrim 'TyBool) 0) (Task $ View (S "done")) (Task $ Fail)
  )


machine :: Pretask ('TyTask ('TyPrim 'TyString))
machine =
  Enter @'TyInt :>>\
  If (Bn Eq (Var @('TyPrim 'TyInt) 0) (I 1)) (Task $ View (S "Biscuit")) (
  If (Bn Eq (Var @('TyPrim 'TyInt) 0) (I 2)) (Task $ View (S "Chocolate")) (
  Task $ Fail))



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
--- new version with cutof
[,(    , ⊠ ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))        , ((True ∧ True) ∧ (True ∧ True)))
 ,(F s0, □(s0) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , □(s0) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(S s1, ⊠ ⋈ □(s1) ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , ⊠ ⋈ □(s1) ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(F s2, □(s2) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , □(s2) ⋈ ⊠ ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
 ,(S s3, □(s0) ⋈ □(s3) ▶ λ.□((fst x0 + snd x0)), True)
 ,(    , □((s0 + s3))                          , ((True ∧ True) ∧ ((True ∧ True) ∧ ((True ∧ (True ∧ True)) ∧ ((True ∧ True) ∧ (True ∧ True))))) )
 ,(F s4, □(s4) ⋈ □(s1) ▶ λ.□((fst x0 + snd x0)), True)
 ,(    , □((s4 + s1))                          , ((True ∧ True) ∧ ((True ∧ True) ∧ ((True ∧ (True ∧ True)) ∧ ((True ∧ True) ∧ (True ∧ True))))) )
 ,(S s5, ⊠ ⋈ □(s5) ▶ λ.□((fst x0 + snd x0))    , True)
 ,(    , ⊠ ⋈ □(s5) ▶ λ.□((fst x0 + snd x0))    , ((True ∧ True) ∧ (True ∧ True)))
]

>>> print $ pretty $ exec $ run Test.Exprs.add_par
[ ( □((s0 + s3)) , [S s3, F s0] , ((((True ∧ True) ∧ (True ∧ True)) ∧ (True ∧ ((True ∧ True) ∧ (True ∧ True)))) ∧ (True ∧ ((True ∧ True) ∧ ((True ∧ True) ∧ ((True ∧ (True ∧ True)) ∧ ((True ∧ True) ∧ (True ∧ True))))))) )
, ( □((s4 + s1)) , [F s4, S s1] , ((((True ∧ True) ∧ (True ∧ True)) ∧ (True ∧ ((True ∧ True) ∧ (True ∧ True)))) ∧ (True ∧ ((True ∧ True) ∧ ((True ∧ True) ∧ ((True ∧ (True ∧ True)) ∧ ((True ∧ True) ∧ (True ∧ True))))))) )
]
-}


-- [,(  , ⊠ ▷… λ.□(x0)    , (True ∧ True))
--  ,(s0, □(s0) ▷… λ.□(x0), True)
--  ,(  , □(s0) ▷… λ.□(x0), (True ∧ True))
--  ,(s1, □(s1) ▷… λ.□(x0), True)
--  ,(  , □(s1) ▷… λ.□(x0), (True ∧ True))
-- ]
