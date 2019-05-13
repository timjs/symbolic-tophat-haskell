module Test.Exprs where


import Language.Expr
import Language.Expr.Simulate



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



{- Tasks --

enterInt' :: Pretask ('TyTask ('TyPrim 'TyInt))
enterInt' =
  Enter


echo :: Pretask ('TyTask ('TyPrim 'TyInt))
echo =
  Enter :>>=
  View (Var 0)


add_seq :: Pretask ('TyTask ('TyPrim 'TyInt))
add_seq =
  Enter :>>=
  Enter :>>=
  View (Bn Add (Var 0) (Var 1))


add_par :: Pretask ('TyTask ('TyPrim 'TyInt))
add_par = Enter :&&: Enter :>>=
  View (Bn Add (Fst (Var 0)) (Snd (Var 0)))

-}


-- Main ------------------------------------------------------------------------


main :: IO ()
main = do
  let result = eval $ App double_mul (I 3)
  print $ pretty result
