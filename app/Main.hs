module Main where


import Language.Expr


-- Examples --------------------------------------------------------------------


double_mul :: Expr cxt ('TyInt ':-> 'TyInt)
double_mul = Lam (Bin Mul (Val 2) (Var Here))


double_add :: Expr cxt ('TyInt ':-> 'TyInt)
double_add = Lam (Bin Add (Var Here) (Var Here))


fact :: Expr cxt ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Bin Eq (Var Here) (Val 0))
    (Val 1)
    (Bin Mul (App fact (Bin Sub (Var Here) (Val 1))) (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ()
main = putText "Hello world!"
