module Examples where

import Tophat.Expr


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
  Update (I 0) :&&: Update (I 0) :>>!
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
      Task (Update x) `Next` Lam (
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
  If (B True) (Task $ Update $ S "Biscuit") (Task $ Update $ S "Chocolate")


step_fail :: Expr ('TyTask ('TyPrim 'TyInt)) -- Int because otherwise not Typeable
step_fail = Task $
  Enter @'TyInt :>>= Fail


iffail :: Expr ('TyTask ('TyPrim 'TyString))
iffail = Task $
  Enter @'TyInt :>>!
  If (B True) (Task $ Update $ S "Biscuit") (Task $ Fail)


share :: Expr ('TyTask ('TyPrim 'TyInt))
share =
  Let (Ref (I 0)) $ Task $
  Change @'TyInt (Var 0)


share' :: Expr ('TyTask ('TyPrim 'TyInt))
share' = Task $
  Change @'TyInt (Ref (I 0))


shareStep :: Expr ('TyTask ('TyPrim 'TyString))
shareStep = Task $
  Change @'TyInt (Ref (I 0)) :>>!
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "done")) (Task $ Fail)


shareStepCont :: Expr ('TyTask ('TyPrim 'TyString))
shareStepCont =
  Let (Ref (I 0)) $ Task $
  Change @'TyInt (Var 0) :>>?
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "done")) (Task $ Fail)


-- Shares --

done :: Expr ('TyTask ('TyPrim 'TyString))
done   = Task $ View (S "done")


stop :: Expr ('TyTask a)
stop   = Task $ Fail


share_step :: Expr ('TyTask TyPrimString)
share_step = Task $
  Change (Ref (I 0)) :>>!
  If (Bn Gt (Var 0) (I 0)) done stop


share_par :: Expr ('TyTask (TyPrimInt ':>< TyPrimInt))
share_par = Task $
  Change (Ref (I 0)) :&&: Change (Ref (I 0))


share_par_step :: Expr ('TyTask TyPrimString)
share_par_step = Task $
  Change (Ref (I 0)) :&&: Change (Ref (I 0)) :>>!
  If (Bn Gt (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0))) done stop



type TyLock = 'TyPrim 'TyString ':>< ('TyPrim 'TyUnit ':>< 'TyPrim 'TyUnit)

lock :: Expr ('TyTask TyLock)
lock =
  Let (Ref (I 0)) $  -- k
  Let (Ref (I 0)) $  -- c
  Task $
  door :&&: (unlock 1 :&&: unlock 2)
  where
    door     = Change @'TyInt (Var 1)  :>>! If (Bn Eq (Deref (Var 1)) (I 2)) done stop
    unlock n = Update U :>>! If (Bn Eq (Deref (Var 2)) (I n)) (inc (Var 1)) stop
    inc c    = Task $ View $ Assign c (Bn Add (Deref c) (I 1))
