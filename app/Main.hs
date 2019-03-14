module Main where


import Language.Expr


-- Examples --------------------------------------------------------------------


double_mul :: Expr cxt '[] ('TyInt ':-> 'TyInt)
double_mul = Lam (Bn Mul (Val 2) (Var Here))


double_add :: Expr cxt '[] ('TyInt ':-> 'TyInt)
double_add = Lam (Bn Add (Var Here) (Var Here))


abs :: Expr cxt '[] ('TyInt ':-> 'TyInt)
abs = Lam
  (If (Bn Lt (Var Here) (Val 0))
    (Un Neg (Var Here))
    (Var Here))


abs' :: Expr cxt '[ 'TyInt ] 'TyInt
abs' =
  If (Bn Lt (Sym Here) (Val 0))
    (Un Neg (Sym Here))
    (Sym Here)


fact :: Expr cxt '[] ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Bn Eq (Var Here) (Val 0))
    (Val 1)
    (Bn Mul (App fact (Bn Sub (Var Here) (Val 1))) (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ()
main = putText "Hello world!"
