module Main where


import Language.Expr


-- Examples --------------------------------------------------------------------


cDoubleMul :: Expr cxt '[] ('TyInt ':-> 'TyInt)
cDoubleMul = Lam (Bn Mul (Val 2) (Var Here))


cDoubleAdd :: Expr cxt '[] ('TyInt ':-> 'TyInt)
cDoubleAdd = Lam (Bn Add (Var Here) (Var Here))


cAbs :: Expr cxt '[] ('TyInt ':-> 'TyInt)
cAbs = Lam
  (If (Bn Lt (Var Here) (Val 0))
    (Un Neg (Var Here))
    (Var Here))


sAbs :: Expr cxt '[ 'TyInt ] 'TyInt
sAbs =
  If (Bn Lt (Sym Here) (Val 0))
    (Un Neg (Sym Here))
    (Sym Here)


cFact :: Expr cxt '[] ('TyInt ':-> 'TyInt)
cFact = Lam
  (If (Bn Eq (Var Here) (Val 0))
    (Val 1)
    (Bn Mul (App cFact (Bn Sub (Var Here) (Val 1))) (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ()
main = putText "Hello world!"
