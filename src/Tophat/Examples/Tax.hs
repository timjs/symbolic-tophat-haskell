module Tophat.Examples.Tax where

import Tophat.Expr


type Address = 'TyPrimString
type Date = 'TyPrimInt
type Amount = 'TyPrimInt
type Boolean = 'TyPrimBool

type AddressDate = TyPair Address Date
type AmountDateBoolean = TyPair ('TyPrimPair Amount Date) Boolean


tax :: Pretask ('TyTask ('TyPrim ('TyPrimPair ('TyPrimPair ('TyPrimPair ('TyPrimPair ('TyPrimPair 'TyPrimInt Boolean) Boolean) Amount) Date) Date)))
tax = let
    provideCitizenInfo :: Pretask ('TyTask ('TyPrim ('TyPrimPair Address Date)))
    provideCitizenInfo = Enter @Address :&&: Enter @Date

    provideDocuments :: Pretask ('TyTask ('TyPrim ('TyPrimPair Amount Date)))
    provideDocuments = Enter @Amount :&&: Enter @Date

    companyConfirm :: Pretask ('TyTask ('TyPrim Boolean))
    companyConfirm = Update (B True) :??: Update (B False)

    officerApprove :: Expr ('TyPrim Date ':-> 'TyPrim Date ':-> 'TyPrim Boolean ':-> 'TyTask TyBool)
    officerApprove =
      Lam @('TyPrim Date)    $ -- 2: invoiceDate
      Lam @('TyPrim Date)    $ -- 1: today
      Lam @('TyPrim Boolean) $ -- 0: confirmed
      Task (Update (B False) :??: Wrap (If (Bn Conj (Bn Lt (Bn Sub (Var 1) (Var 2)) (I 365)) (Var 0))
        (Task $ Update (B True))
        (Task $ Fail)))
  in
  provideCitizenInfo :>>=  -- 0: <address, today>
  provideDocuments :&&: companyConfirm :>>= -- 0: <<invoiceAmount, invoiceDate>, confirmed>
  Wrap (App (App (App officerApprove
    (Snd (Fst (Var @AmountDateBoolean 0))) )
    (Snd (Var @AddressDate 1)) )
    (Snd (Var @AmountDateBoolean 0))) :>>= -- 0: decision
  let
    subsidyAmount = If (Var @('TyPrim Boolean) 0)
      (Bn Min (I 600) (Bn Div (Fst (Fst (Var @AmountDateBoolean 1))) (I 10)))
      (I 0)
  in
  View (subsidyAmount :*: Var @('TyPrim Boolean) 0 :*: Snd (Var @AmountDateBoolean 1) :*: Fst (Fst (Var @AmountDateBoolean 1)) :*: Snd (Fst (Var @AmountDateBoolean 1)) :*: Snd (Var @AddressDate 2))
