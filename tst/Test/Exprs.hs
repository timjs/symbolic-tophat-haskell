module Test.Exprs where


import Data.SBV

import Language.Expr
import qualified Language.Expr.Symb as Symb



-- Examples --------------------------------------------------------------------

-- Functions --


double_mul :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
double_mul = Lam (Bn Mul (I 2) (Var Here))


double_add :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
double_add = Lam (Bn Add (Var Here) (Var Here))


abs :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
abs = Lam
  (If (Bn Lt (Var Here) (I 0))
    (Un Neg (Var Here))
    (Var Here))


add :: Expr cxt sxt ('TyPrim 'TyInt ':-> ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt))
add = Lam (Lam (
  Bn Add (Var Here) (Var (There Here))))


fact :: Expr cxt '[] ('TyPrim 'TyInt ':-> 'TyPrim 'TyInt)
fact = Lam
  (If (Bn Eq (Var Here) (I 0))
    (I 1)
    (Bn Mul (App fact (Bn Sub (Var Here) (I 1))) (Var Here)))



-- Tasks --

enterInt' :: Pretask '[] '[] ('TyTask ('TyPrim 'TyInt))
enterInt' =
  Enter


echo :: Pretask '[] '[] ('TyTask ('TyPrim 'TyInt))
echo =
  Enter :>>=
  View (Var Here)


add_seq :: Pretask '[] '[] ('TyTask ('TyPrim 'TyInt))
add_seq =
  Enter :>>=
  Enter :>>=
  View (Bn Add (Var Here) (Var (There Here)))


add_par :: Pretask '[] '[] ('TyTask ('TyPrim 'TyInt))
add_par = Enter :&&: Enter :>>=
  View (Bn Add (Fst (Var Here)) (Snd (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ()
main = do
  result <- prove \x y -> Symb.eval'' add x y .== x + y
  print result
