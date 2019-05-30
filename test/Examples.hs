module Examples where

import Tophat.Expr


-- Examples --------------------------------------------------------------------

-- Functions --


double_mul :: Expr (TyInt ':-> TyInt)
double_mul = Lam (Bn Mul (I 2) (Var 0))


double_add :: Expr (TyInt ':-> TyInt)
double_add = Lam (Bn Add (Var 0) (Var 0))


abs :: Expr (TyInt ':-> TyInt)
abs = Lam
  (If (Bn Lt (Var 0) (I 0))
    (Un Neg (Var 0))
    (Var 0))


add :: Expr (TyInt ':-> (TyInt ':-> TyInt))
add = Lam (Lam (
  Bn Add (Var 0) (Var 1)))


fact :: Expr (TyInt ':-> TyInt)
fact = Lam
  (If (Bn Eq (Var 0) (I 0))
    (I 1)
    (Bn Mul (App fact (Bn Sub (Var 0) (I 1))) (Var 0)))



-- Tasks --

echo :: Expr ('TyTask TyInt)
echo = Task $
  Enter @'TyPrimInt :>>=
  View (Var 0)


echo' :: Expr ('TyTask TyInt)
echo' = Task $
  Enter @'TyPrimInt :>>?
  Task (View (Var 0))


add_seq :: Expr ('TyTask TyInt)
add_seq = Task $
  Enter @'TyPrimInt :>>=
  Enter @'TyPrimInt :>>=
  View (Bn Add (Var 0) (Var 1))


add_seq' :: Expr ('TyTask TyInt)
add_seq' = Task $
  Enter @'TyPrimInt :>>=
  Enter @'TyPrimInt :>>=
  View (Bn Add (Var 0) (Var 0))


cons_list :: Expr ('TyTask (TyList 'TyPrimInt))
cons_list =
  Let (Cons (I 1) (Cons (I 2) Nil)) $ Task $
  Enter @'TyPrimInt :>>=
  View (Cons (Var 0) (Var 1))
{-
⊠ ▶ λ.□(x0 :: 1 :: 2 :: [])
s0 => ( ( □(s0 :: 1 :: 2 :: []) , [s0] , True ) , <> )
-}


add_par :: Expr ('TyTask (TyPair ('TyPrimPair 'TyPrimInt 'TyPrimInt) 'TyPrimInt))
add_par = Task $
  Enter @'TyPrimInt :&&: Enter @'TyPrimInt :>>=
  View (let
    x = Fst (Var @TyIntInt 0)
    y = Snd (Var @TyIntInt 0)
  in Bn Add x y :*: x :*: y)


par_step :: Expr ('TyTask TyString)
par_step = Task $
  Update (I 0) :&&: Update (I 0) :>>!
  If (Bn Gt (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0))) done stop


guard0 :: Expr ('TyTask TyInt)
guard0 = Task $
  Enter @'TyPrimInt :>>!
  If (Bn Gt (Var 0) (I 0)) (Task $ View (Var 0)) (Task $ Fail)


preguard :: Expr ('TyTask (TyString))
preguard = Task $
  Enter @'TyPrimBool :>>!
  If (Un Not (Var 0)) (contguard (Var 0)) (Task $ Fail)
  where
    contguard :: Expr TyBool -> Expr ('TyTask (TyString))
    contguard x = Task $
      Task (Update x) `Next` Lam (
        If (Var @TyBool 0) (Task $ View (S "done")) (Task $ Fail)
      )


machine :: Expr ('TyTask (TyString))
machine = Task $
  Enter @'TyPrimInt :>>!
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "Biscuit")) (
  If (Bn Eq (Var 0) (I 2)) (Task $ View (S "Chocolate")) (
  Task $ Fail))


iftest :: Expr ('TyTask (TyString))
iftest =
  If (B True) (Task $ Update $ S "Biscuit") (Task $ Update $ S "Chocolate")


step_fail :: Expr ('TyTask TyInt) -- Int because otherwise not Typeable
step_fail = Task $
  Enter @'TyPrimInt :>>= Fail


iffail :: Expr ('TyTask (TyString))
iffail = Task $
  Enter @'TyPrimInt :>>!
  If (B True) (Task $ Update $ S "Biscuit") (Task $ Fail)


share :: Expr ('TyTask TyInt)
share =
  Let (Ref (I 0)) $ Task $
  Change @'TyPrimInt (Var 0)


share' :: Expr ('TyTask TyInt)
share' = Task $
  Change @'TyPrimInt (Ref (I 0))


shareStep :: Expr ('TyTask TyString)
shareStep = Task $
  Change @'TyPrimInt (Ref (I 0)) :>>!
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "done")) (Task $ Fail)


shareStepCont :: Expr ('TyTask TyString)
shareStepCont =
  Let (Ref (I 0)) $ Task $
  Change @'TyPrimInt (Var 0) :>>?
  If (Bn Eq (Var 0) (I 1)) (Task $ View (S "done")) (Task $ Fail)


-- Shares --

done :: Expr ('TyTask TyString)
done   = Task $ View (S "done")


stop :: Expr ('TyTask a)
stop   = Task $ Fail


share_step :: Expr ('TyTask TyString)
share_step = Task $
  Change (Ref (I 0)) :>>!
  If (Bn Gt (Var 0) (I 0)) done stop


share_par :: Expr ('TyTask TyIntInt)
share_par = Task $
  Change (Ref (I 0)) :&&: Change (Ref (I 0))


share_par_step :: Expr ('TyTask TyString)
share_par_step = Task $
  Change (Ref (I 0)) :&&: Change (Ref (I 0)) :>>!
  If (Bn Gt (Fst (Var @TyIntInt 0)) (Snd (Var @TyIntInt 0))) done stop



type TyLock = TyPair 'TyPrimString ('TyPrimPair 'TyPrimUnit 'TyPrimUnit)

lock :: Expr ('TyTask TyLock)
lock =
  Let (Ref (I 0)) $  -- k
  Let (Ref (I 0)) $  -- c
  Task $
  door :&&: (unlock 1 :&&: unlock 2)
  where
    door     = Change @'TyPrimInt (Var 1)  :>>! If (Bn Eq (Deref (Var 1)) (I 2)) done stop
    unlock n = Update U :>>! If (Bn Eq (Deref (Var 2)) (I n)) (inc (Var 1)) stop
    inc c    = Task $ View $ Assign c (Bn Add (Deref c) (I 1))
