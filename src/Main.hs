module Main where

import Tophat.Expr.Run

import Tophat.Examples.Flight
import Tophat.Examples.Tax


main :: IO ()
main = do
  writeFile "Flight.out" (show $ pretty $ execSimulation $ initialise flight)
  writeFile "Tax.out"    (show $ pretty $ execSimulation $ initialise tax)
