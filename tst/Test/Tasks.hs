module Test.Tasks where


import Data.Task


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
