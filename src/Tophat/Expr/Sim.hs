module Tophat.Expr.Sim
  ( Simulation, runSimulation, evalSimulation
  ) where

import Control.Monad.Root
import Control.Monad.Supply
import Tophat.Heap
import Tophat.Input

import Data.Root (Root)
import Data.Stream (Stream)

import qualified Data.Stream as Stream


ids :: Stream Nat
ids = Stream.iterate succ 0


type Simulation = StateT Heap (RootT (List Input) (SupplyT Nat Identity))

runSimulation :: Simulation a -> ( Root (List Input) ( a, Heap ), Stream Nat )
runSimulation r = runIdentity (runSupplyT (runRootT (runStateT r empty)) ids)

evalSimulation :: Simulation a -> Root (List Input) ( a, Heap )
evalSimulation = fst << runSimulation
