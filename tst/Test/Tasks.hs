module Test.Tasks where


import Data.SBV
import Data.Task
import Data.Task.Simulate



-- Concrete --------------------------------------------------------------------


addSeq :: Task Integer
addSeq =
  enter >>- \x ->
  enter >>- \y ->
  view (x + y)


addPar :: Task Integer
addPar =
  enter -&&- enter >>- \( x, y ) ->
  view (x + y)


guarded :: Task Integer
guarded =
  enter >>- \x ->
  if x > 0
    then view x
    else fail


vending :: Task String
vending =
  enter >>- \(n :: Integer) ->
  case n of
    1 -> view "Biscuit"
    2 -> view "Chocolate"
    _ -> fail



-- Symbolic --------------------------------------------------------------------


addSeqS :: SInteger -> SInteger -> Task SInteger
addSeqS s0 s1 =
  edit s0 >>- \x ->
  edit s1 >>- \y ->
  view (x + y)


addParS :: SInteger -> SInteger -> Task SInteger
addParS s0 s1 =
  edit s0 -&&- edit s1 >>- \( x, y ) ->
  view (x + y)


-- guardedS :: SInteger -> Task SInteger
-- guardedS s0 =
--   edit s0 >>- \x ->
--   ite (x .> 0)
--     (view x)
--     fail


-- vendingS :: SInteger -> Task SString
-- vendingS s0 =
--   edit s0 >>- \n ->
--   ite (s0 .== 1)
--     (view "Biscuit")
--     (ite (s0 .== 2)
--       (view "Chocolate")
--       fail)
