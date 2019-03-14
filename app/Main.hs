module Main where


import Language.Expr


-- Examples --------------------------------------------------------------------


double_mul :: Expr cxt sxt ('TyInt ':-> 'TyInt)
double_mul = Lam (Bin Mul (Val 2) (Var Here))


double_add :: Expr cxt sxt ('TyInt ':-> 'TyInt)
double_add = Lam (Bin Add (Var Here) (Var Here))


abs :: Expr cxt sxt ('TyInt ':-> 'TyInt)
abs = Lam
  (If (Bin Lt (Var Here) (Val 0))
    (Un Neg (Var Here))
    (Var Here))


abs' :: Expr cxt '[ 'TyInt ] 'TyInt
abs' =
  If (Bin Lt (Sym Here) (Val 0))
    (Un Neg (Sym Here))
    (Sym Here)


fact :: Expr cxt sxt ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Bin Eq (Var Here) (Val 0))
    (Val 1)
    (Bin Mul (App fact (Bin Sub (Var Here) (Val 1))) (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ()
main = putText "Hello world!"
