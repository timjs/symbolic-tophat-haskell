module Test.Solver where


import Data.SBV

import qualified Test.Exprs
import qualified Test.Tasks



intInSymEither :: SInteger -> SEither Integer Integer
intInSymEither x =
  ite (x .> 0)
    (literal $ Right 1)
    (literal $ Left 1)


sRight :: SEither Integer Integer
sRight = literal $ Right 1


symIntInEither :: SInteger -> Either SInteger SInteger
symIntInEither x =
  ite (x .> 0)
    (Right 1)
    (Left 1)


cRight :: Either SInteger SInteger
cRight = Right 1



main :: IO ()
main = do
  putStrLn "==================================================================="
  r <- proveWith (z3 {verbose = True}) \ x -> intInSymEither x .== sRight
  print r
  r <- proveWith (z3 {verbose = True}) \ x -> symIntInEither x .== cRight
  print r
  putStrLn "==================================================================="
