module Tophat.Examples.Tax where

import Tophat.Expr


type Address = 'TyPrimString
type Date = 'TyPrimInt
type Amount = 'TyPrimInt
type Boolean = 'TyPrimBool

type AddressDate = TyPair Address Date
type AmountDateBoolean = TyPair ('TyPrimPair Amount Date) Boolean

test :: Expr
          ('TyTask
             ('TyPrim
                ('TyPrimPair
                   ('TyPrimPair
                      ('TyPrimPair
                         ('TyPrimPair ('TyPrimPair 'TyPrimInt Boolean) Boolean) Amount)
                      Date)
                   Date)))
test = let
    provideCitizenInfo :: Expr ('TyTask ('TyPrim ('TyPrimPair Address Date)))
    provideCitizenInfo = Task $ Enter @('TyPrimPair Address Date)
    provideDocuments :: Expr ('TyTask ('TyPrim ('TyPrimPair Amount Date)))
    provideDocuments = Task $ Enter @('TyPrimPair Amount Date)
    companyConfirm :: Expr ('TyTask ('TyPrim Boolean))
    companyConfirm = Task $ Task (Update (B True)) :??: Task (Update (B False))
    officerApprove :: Expr ('TyPrim Date ':-> 'TyPrim Date ':-> 'TyPrim Boolean ':-> 'TyTask TyBool)
    officerApprove =
      Lam @('TyPrim Date)    $ -- 2: invoiceDate
      Lam @('TyPrim Date)    $ -- 1: today
      Lam @('TyPrim Boolean) $ -- 0: confirmed
      Task $ Task (Update (B False)) :??: If (Bn Conj (Bn Lt (Bn Sub (Var 1) (Var 2)) (I 365)) (Var 0))
        (Task $ Update (B True))
        (Task $ Fail)
  in
  Task (provideCitizenInfo :>>= Task ( -- 0: <address, today>
  Task (provideDocuments :&&: companyConfirm) :>>= Task (-- 0: <<invoiceAmount, invoiceDate>, confirmed>
  App (App (App officerApprove
    (Fst (Fst (Var @AmountDateBoolean 0))) )
    (Snd (Var @AddressDate 1)) )
    (Snd (Var @AmountDateBoolean 0)) :>>= -- 0: decision
  let
    subsidyAmount = If (Var @('TyPrim Boolean) 0)
      (Bn Min (I 600) (Bn Div (Fst (Fst (Var @AmountDateBoolean 1))) (I 10)))
      (I 0)
  in
  Task (View (subsidyAmount :*: Var @('TyPrim Boolean) 0 :*: Snd (Var @AmountDateBoolean 1) :*: Fst (Fst (Var @AmountDateBoolean 1)) :*: Snd (Fst (Var @AmountDateBoolean 1)) :*: Snd (Var @AddressDate 2)))
  )))
