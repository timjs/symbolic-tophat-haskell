module Tophat.Examples.Vending where

import Tophat.Expr


type Snack = 'TyPrimString
type Money = 'TyPrimInt


vend :: Pretask ('TyTask ('TyPrim Snack))
vend =
  Update (I 0) :>>=  -- 0: n
    Wrap (If (Bn Eq (Var 0) (I 1))
      (Task (Update (S "Biscuit")))
      (If (Bn Eq (Var 0) (I 2))
        (Task (Update (S "ChocolateBar")))
        (Task Fail)
      )
    )
