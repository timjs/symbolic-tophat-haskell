module Main where


import Language.Expr


-- Examples --------------------------------------------------------------------


cDoubleMul :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
cDoubleMul = Lam (Bn Mul (Con 2) (Var Here))


cDoubleAdd :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
cDoubleAdd = Lam (Bn Add (Var Here) (Var Here))


cAbs :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
cAbs = Lam
  (If (Bn Lt (Var Here) (Con 0))
    (Un Neg (Var Here))
    (Var Here))


sAbs :: Expr cxt '[ 'TyInt ] ('TyPrim 'TyInt)
sAbs =
  If (Bn Lt (Sym Here) (Con 0))
    (Un Neg (Sym Here))
    (Sym Here)


cFact :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
cFact = Lam
  (If (Bn Eq (Var Here) (Con 0))
    (Con 1)
    (Bn Mul (App cFact (Bn Sub (Var Here) (Con 1))) (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ()
main = putText "Hello world!"
