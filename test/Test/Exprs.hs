module Test.Exprs where

import Data.Some (Some)
import Data.Steps (Steps)
import Data.Stream (Stream)
import Language.Val (Val)

import Control.Monad.List
import Control.Monad.Supply
import Control.Monad.Steps
import Control.Monad.Writer.Strict
import Language.Expr
import Language.Store

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

echo :: Expr ('TyTask ('TyPrim 'TyInt))
echo = Task $
  Enter @'TyInt :>>=
  View (Var 0)


echo' :: Expr ('TyTask ('TyPrim 'TyInt))
echo' = Task $
  Enter @'TyInt :>>?
  Task (View (Var 0))


add_seq :: Expr ('TyTask ('TyPrim 'TyInt))
add_seq = Task $
  Enter @'TyInt :>>=
  Enter @'TyInt :>>=
  View (Bn Add (Var 0) (Var 1))


add_seq' :: Expr ('TyTask ('TyPrim 'TyInt))
add_seq' = Task $
  Enter @'TyInt :>>=
  Enter @'TyInt :>>=
  View (Bn Add (Var 0) (Var 0))


type TyIntInt = 'TyPrim 'TyInt ':>< 'TyPrim 'TyInt

add_par :: Expr ('TyTask ('TyPrim 'TyInt))
add_par = Task $
  Enter @'TyInt :&&: Enter @'TyInt :>>=
  View (Bn Add (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0)))


par_step :: Expr ('TyTask TyPrimString)
par_step = Task $
  Edit (I 0) :&&: Edit (I 0) :>>!
  If (Bn Gt (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0))) done stop


guard0 :: Expr ('TyTask ('TyPrim 'TyInt))
guard0 = Task $
  Enter @'TyInt :>>!
  If (Bn Gt (Var 0) (I 0)) (Task $ View (Var 0)) (Task $ Fail)


preguard :: Expr ('TyTask ('TyPrim 'TyString))
preguard = Task $
  Enter @'TyBool :>>!
  If (Un Not (Var 0)) (contguard (Var 0)) (Task $ Fail)
  where
    contguard :: Expr ('TyPrim 'TyBool) -> Expr ('TyTask ('TyPrim 'TyString))
    contguard x = Task $
      Task (Edit x) `Next` Lam (
        If (Var @('TyPrim 'TyBool) 0) (Task $ View (S "done")) (Task $ Fail)
      )


machine :: Expr ('TyTask ('TyPrim 'TyString))
machine = Task $
  Enter @'TyInt :>>!
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "Biscuit")) (
  If (Bn Eq (Var 0) (I 2)) (Task $ View (S "Chocolate")) (
  Task $ Fail))


iftest :: Expr ('TyTask ('TyPrim 'TyString))
iftest =
  If (B True) (Task $ Edit $ S "Biscuit") (Task $ Edit $ S "Chocolate")


step_fail :: Expr ('TyTask ('TyPrim 'TyInt)) -- Int because otherwise not Typeable
step_fail = Task $
  Enter @'TyInt :>>= Fail


iffail :: Expr ('TyTask ('TyPrim 'TyString))
iffail = Task $
  Enter @'TyInt :>>!
  If (B True) (Task $ Edit $ S "Biscuit") (Task $ Fail)


share :: Expr ('TyTask ('TyPrim 'TyInt))
share =
  Let (Ref (I 0)) $ Task $
  Update @'TyInt (Var 0)


share' :: Expr ('TyTask ('TyPrim 'TyInt))
share' = Task $
  Update @'TyInt (Ref (I 0))


shareStep :: Expr ('TyTask ('TyPrim 'TyString))
shareStep = Task $
  Update @'TyInt (Ref (I 0)) :>>!
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "done")) (Task $ Fail)


shareStepCont :: Expr ('TyTask ('TyPrim 'TyString))
shareStepCont =
  Let (Ref (I 0)) $ Task $
  Update @'TyInt (Var 0) :>>?
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "done")) (Task $ Fail)


-- Shares --

done :: Expr ('TyTask ('TyPrim 'TyString))
done   = Task $ View (S "done")


stop :: Expr ('TyTask a)
stop   = Task $ Fail


share_step :: Expr ('TyTask TyPrimString)
share_step = Task $
  Update (Ref (I 0)) :>>!
  If (Bn Gt (Var 0) (I 0)) done stop


share_par :: Expr ('TyTask (TyPrimInt ':>< TyPrimInt))
share_par = Task $
  Update (Ref (I 0)) :&&: Update (Ref (I 0))


share_par_step :: Expr ('TyTask TyPrimString)
share_par_step = Task $
  Update (Ref (I 0)) :&&: Update (Ref (I 0)) :>>!
  If (Bn Gt (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0))) done stop



type TyLock = 'TyPrim 'TyString ':>< ('TyPrim 'TyUnit ':>< 'TyPrim 'TyUnit)

lock :: Expr ('TyTask TyLock)
lock =
  Let (Ref (I 0)) $  -- k
  Let (Ref (I 0)) $  -- c
  Task $
  door :&&: (unlock 1 :&&: unlock 2)
  where
    door     = Update @'TyInt (Var 1)  :>>! If (Bn Eq (Deref (Var 1)) (I 2)) done stop
    unlock n = Edit U :>>! If (Bn Eq (Deref (Var 2)) (I n)) (inc (Var 1)) stop
    inc c    = Task $ View $ Assign c (Bn Add (Deref c) (I 1))



-- Main ------------------------------------------------------------------------


ids :: Stream Nat
ids = Stream.iterate succ 0


type Runner = ListT (SupplyT Nat (WriterT (List (Doc ())) Store))

runRunner :: Runner a -> ( ( ( List a, Stream Nat ), List (Doc ()) ), List (Some Val) )
runRunner r = runStore (runWriterT (runSupplyT (runListT r) ids))

traceRunner :: Runner a -> List (Doc ())
traceRunner = snd << fst << runRunner

evalRunner :: Runner a -> List a
evalRunner = fst << fst << fst << runRunner

execRunner :: Runner a -> List (Some Val)
execRunner = snd << runRunner


type Stepper = StepsT () (SupplyT Nat (Store))

runStepper :: Stepper a -> ( ( Steps () a, Stream Nat ), List (Some Val) )
runStepper r = runStore (runSupplyT (runStepsT r) ids)

evalStepper :: Stepper a -> Steps () a
evalStepper = fst << fst << runStepper

execStepper :: Stepper a -> List (Some Val)
execStepper = snd << runStepper


{-
>>> print $ trace $ run echo
[(, ⊠ ▶ λ.□(x0), (True ∧ True))
,(s0, □(s0) ▶ λ.□(x0), True)
,(, □(s0), (True ∧ (True ∧ ((True ∧ True) ∧ True))))
]

>>> traceRunner $ run add_seq
[,(  , ⊠ ▶ λ.⊠ ▶ λ.□((x0 + x1))    , (True ∧ True))
 ,(s0, □(s0) ▶ λ.⊠ ▶ λ.□((x0 + x1)), True)
 ,(  , ⊠ ▶ λ.□((x0 + s0))          , (True ∧ (True ∧ ((True ∧ True) ∧ True))))
 ,(s1, □(s1) ▶ λ.□((x0 + s0))      , True)
 ,(  , □((s1 + s0))                , (True ∧ (True ∧ ((True ∧ True) ∧ (True ∧ True)))))
]

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

>>> pretty machine
⊠ ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯

>>> pretty $ evalRunner $ run machine
, ( □(Biscuit), [s0], s0 == 1 )
, ( □(Biscuit), [s0], s0 == 1 )

, ( □(Biscuit), [s0, s1], s1 == 1 )
, ( □(Biscuit), [s0, s1], s1 == 1 )

, ( □(Biscuit), [s0, s1, s2], s2 == 1 )
, ( □(Biscuit), [s0, s1, s2], s2 == 1 )

, ( □(Chocolate), [s0], s0 == 2 ∧ not (s0 == 1) )
, ( □(Chocolate), [s0, s1], s1 == 2 ∧ not (s1 == 1) )
, ( □(Chocolate), [s0, s1, s2], s2 == 2 ∧ not (s2 == 1) )

>>> traceRunner $ run machine
[,(    , ⊠ ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯     , (True ∧ True) )

 ,( s0 , □(s0) ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯ , True )
 ,(    , □(Biscuit)                                                                        , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s0 == 1))))) )
 ,(    , □(Chocolate)                                                                      , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s0 == 2))) ∧ (not (s0 == 1)))))) )
 ,(    , □(Biscuit)                                                                        , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s0 == 1))))) )
 ,(    , □(s0) ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯ , (True ∧ True) )

 ,( s1 , □(s1) ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯ , True )
 ,(    , □(Biscuit)                                                                        , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s1 == 1))))) )
 ,(    , □(Chocolate)                                                                      , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s1 == 2))) ∧ (not (s1 == 1)))))) )
 ,(    , □(Biscuit)                                                                        , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s1 == 1))))) )
 ,(    , □(s1) ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯ , (True ∧ True) )

 ,( s2 , □(s2) ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯ , True )
 ,(    , □(Biscuit)                                                                        , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s2 == 1))))) )
 ,(    , □(Chocolate)                                                                      , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s2 == 2))) ∧ (not (s2 == 1)))))) )
 ,(    , □(Biscuit)                                                                        , (True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s2 == 1))))) )
 ,(    , □(s2) ▶ λ.if (x0 == 1) then □(Biscuit) else if (x0 == 2) then □(Chocolate) else ↯ , (True ∧ True) )

]

>>> traceRunner $ run machine
[,(    , ⊠ ▶ λ.if (x0 == 1) then □("Biscuit") else if (x0 == 2) then □("Chocolate") else ↯     , (True ∧ True) )
 ,( s0 , □(s0) ▶ λ.if (x0 == 1) then □("Biscuit") else if (x0 == 2) then □("Chocolate") else ↯ , True )
 ,(    , □(Chocolate)                                                                          , ((True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s0 == 2))) ∧ (not (s0 == 1)))))) ∧ (True ∧ True)) )
 ,(    , □(Biscuit)                                                                            , ((True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s0 == 1))))) ∧ (True ∧ True)) )
 ,(    , □(Biscuit)                                                                            , ((True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (s0 == 1))))) ∧ (True ∧ True)) )
]

[(,           , □(Biscuit)   , (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (3 == 1))) ∧ True))
  ,(s0        , □(s0)        , True)
  ,(          , □(s0)        , (True ∧ True))
  ,(          , □(Chocolate) , (((True ∧ True) ∧ (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (3 == 2))) ∧ (not (3 == 1)))) ∧ True) )
  ,(s1        , □(s1)        , True)
  ,(          , □(s1)        , (True ∧ True))
  ,(          , □(Biscuit)   , (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (3 == 1))) ∧ True))
  ,(s2        , □(s2)        , True)
  ,(          , □(s2)        , (True ∧ True))
  ,(          , ↯            , (((True ∧ True) ∧ (((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (not (3 == 2)))) ∧ (not (3 == 1)))) ∧ True) )
]

>>> traceRunner $ run Test.Exprs.shareStep
[( , □("done") , ((True ∧ (True ∧ ((True ∧ True) ∧ (((True ∧ True) ∧ True) ∧ (0 == 1))))) ∧ (True ∧ True)) )
   ,(s0        , □(s0), True)
   ,(          , □(s0), (True ∧ True))]
-}
