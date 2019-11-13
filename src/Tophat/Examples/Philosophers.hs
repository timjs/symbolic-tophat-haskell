module Tophat.Examples.Philosophers where

import Tophat.Expr

import Tophat.Pred (Pred, pattern (:==:))
import Tophat.Val (Val, asPred)

import qualified Tophat.Pred as P


dining :: Pretask ('TyTask ('TyPrim 'TyPrimString))
dining = let
    fork0 :: Expr ('TyRef 'TyPrimBool)
    fork0 = Ref (B True)

    fork1 :: Expr ('TyRef 'TyPrimBool)
    fork1 = Ref (B True)

    fork2 :: Expr ('TyRef 'TyPrimBool)
    fork2 = Ref (B True)

    pickup :: Expr ('TyRef 'TyPrimBool ':-> 'TyRef 'TyPrimBool ':-> 'TyTask ('TyPrim 'TyPrimUnit))
    pickup =
        Lam @('TyRef 'TyPrimBool) $  -- 1: this
        Lam @('TyRef 'TyPrimBool) $  -- 0: that
        If (Deref (Var 1))
          (Task $
            View (Assign (Var 1) (B False)) :>>?
            Wrap (If (Deref (Var 1))
              (Task $ View (Assign (Var 2) (B True)))
              (Task Fail)
            )
          )
          (Task Fail)

    scientist :: Expr ('TyPrim 'TyPrimString ':-> 'TyRef 'TyPrimBool ':-> 'TyRef 'TyPrimBool ':-> 'TyTask ('TyPrim 'TyPrimUnit))
    scientist =
        Lam @('TyPrim 'TyPrimString) $  -- 2: name
        Lam @('TyRef 'TyPrimBool)    $  -- 1: left
        Lam @('TyRef 'TyPrimBool)    $  -- 0: right
        Task                         $ Wrap (App (App pickup (Var 1)) (Var 0)) :??: Wrap (App (App pickup (Var 0)) (Var 1))
  in
  Wrap $
  Let fork0 $  -- 2: fork0
  Let fork1 $  -- 1: fork1
  Let fork2 $  -- 0: fork2
  Task $
    Wrap (App (App (App scientist (S "Alan Turing")) (Var 2)) (Var 1)) :&&:
    Wrap (App (App (App scientist (S "Alan Turing")) (Var 2)) (Var 1)) :&&:
    Wrap (App (App (App scientist (S "Alan Turing")) (Var 2)) (Var 1)) :>>=
    View (S "Full bellies")


goalDining :: Val TyString -> Pred 'TyPrimBool
goalDining v = asPred v :==: P.S "Full bellies"
