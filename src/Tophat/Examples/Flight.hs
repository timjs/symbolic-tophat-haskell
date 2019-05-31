module Tophat.Examples.Flight where

import Tophat.Expr


type TyStore = 'TyRef ('TyPrimList 'TyPrimInt)

flight :: Pretask ('TyTask TyListInt)
flight = let
    maxSeats = I 50
    bookSeat =
      Enter @'TyPrimInt :>>=
      let
        x = Var @TyInt 0
        bookedSeats = Var @TyStore 1
      in
      Wrap (If (Bn Disj (Bn Elem x (Deref bookedSeats)) (Bn Gt x maxSeats))
        (Task Fail)
        (Task $ View (Assign bookedSeats (Cons x (Deref bookedSeats))))
      )
    main =
      bookSeat :&&: bookSeat :>>=
      let
        _x = Var @TyUnit 0
        bookedSeats = Var @TyStore 1
      in
      View (Deref bookedSeats)
  in
  Wrap (Let (Ref L) (Task main))

{-
(λ.
  ⊠(Int) ▶ λ.
    if ((x0 `elem` !x1) ∨ (x0 > 50))
      then ↯
      else □(x1 := x0 :: !x1)
  ⋈
  ⊠(Int) ▶ λ.
    if ((x0 `elem` !x1) ∨ (x0 > 50))
      then ↯
      else □(x1 := x0 :: !x1) ▶ λ.□(!x1)
) (ref [])
-}
