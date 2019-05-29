module Tophat.Expr.Run
  ( Simulation, runSimulation, execSimulation, evalSimulation
  ) where

import Control.Monad.Root
import Control.Monad.Supply

import Data.Root (Root)
import Data.Stream (Stream)
import Tophat.Heap (Heap)
import Tophat.Input (Input)
import Tophat.Pred (Pred(..), Ty(..), PrimTy(..), pattern (:==:), pattern (:/\:))
import Tophat.Val (Val, asPred)

import qualified Data.Stream as Stream


ids :: Stream Nat
ids = Stream.iterate succ 1


type Simulation t = StateT Heap (RootT Text (SupplyT Nat Identity)) ( Val t, List Input, Pred 'TyBool )

runSimulation :: Simulation t -> ( Root Text ( ( Val t, List Input, Pred 'TyBool ), Heap ), Stream Nat )
runSimulation r = runIdentity (runSupplyT (runRootT (runStateT r empty)) ids)

execSimulation :: Simulation t -> Root Text ( Val t, List Input, Pred 'TyBool, Heap )
execSimulation = map go << fst << runSimulation
  where
    go ( ( v, is, p ), h ) = ( v, is, p, h )

evalSimulation :: Simulation ('TyPrim 'TyInt) -> Root Text ( Pred 'TyBool )
evalSimulation = map go << execSimulation
  where
    go ( v, _, p, _ ) = (Sym 0 :==: asPred v) :/\: p
