module Tophat.Expr.Run
  ( Simulation, runSimulation, evalSimulation
  ) where

import Control.Monad.Root
import Control.Monad.Supply
import Tophat.Heap
import Tophat.Input
import Tophat.Pred
import Tophat.Val

import Data.Root (Root)
import Data.Stream (Stream)

import qualified Data.Stream as Stream


ids :: Stream Nat
ids = Stream.iterate succ 0


type Simulation t = StateT Heap (RootT Text (SupplyT Nat Identity)) ( Val ('TyTask t), List Input, Pred 'TyBool )

runSimulation :: Simulation t -> ( Root Text ( ( Val ('TyTask t), List Input, Pred 'TyBool ), Heap ), Stream Nat )
runSimulation r = runIdentity (runSupplyT (runRootT (runStateT r empty)) ids)

evalSimulation :: Simulation t -> Root Text ( ( Val ('TyTask t), List Input, Pred 'TyBool ), Heap )
evalSimulation = fst << runSimulation
