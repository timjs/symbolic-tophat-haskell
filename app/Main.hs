module Main where


import Data.SBV

import Language.Expr
import qualified Language.Expr.Symb as Symb



-- Examples --------------------------------------------------------------------

-- Functions --


double_mul :: Expr cxt '[] ('TyInt ':-> 'TyInt)
double_mul = Lam (Bn Mul (I 2) (Var Here))


double_add :: Expr cxt '[] ('TyInt ':-> 'TyInt)
double_add = Lam (Bn Add (Var Here) (Var Here))


abs :: Expr cxt '[] ('TyInt ':-> 'TyInt)
abs = Lam
  (If (Bn Lt (Var Here) (I 0))
    (Un Neg (Var Here))
    (Var Here))


add :: Expr cxt sxt ('TyInt ':-> ('TyInt ':-> 'TyInt))
add = Lam (Lam (
  Bn Add (Var Here) (Var (There Here))))


fact :: Expr cxt '[] ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Bn Eq (Var Here) (I 0))
    (I 1)
    (Bn Mul (App fact (Bn Sub (Var Here) (I 1))) (Var Here)))



-- Tasks --

enterInt' :: Pretask '[] '[] ('TyTask 'TyInt)
enterInt' =
  Enter


echo :: Pretask '[] '[] ('TyTask 'TyInt)
echo =
  Enter :>>=
  View (Var Here)


add_seq :: Pretask '[] '[] ('TyTask 'TyInt)
add_seq =
  Enter :>>=
  Enter :>>=
  View (Bn Add (Var Here) (Var (There Here)))


add_par :: Pretask '[] '[] ('TyTask 'TyInt)
add_par = Enter :&&: Enter :>>=
  View (Bn Add (Fst (Var Here)) (Snd (Var Here)))



-- Main ------------------------------------------------------------------------


main :: IO ThmResult
main =
  prove \x y -> Symb.eval'' add x y .== x + y
