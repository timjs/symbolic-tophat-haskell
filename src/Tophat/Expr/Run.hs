module Tophat.Expr.Run
  ( Simulation, runSimulation, execSimulation, evalSimulation
  , runFirsts, execFirsts, evalFirsts
  , predicates, predicatesFirsts
  , startSimulate, startFirsts
  ) where

import Control.Monad.Root
import Control.Monad.Supply
import Tophat.Type

import Data.Root (Root)
import Data.Stream (Stream)
import Tophat.Expr.Sim (startSimulate, startFirsts)
import Tophat.Heap (Heap)
import Tophat.Input (Input)
import Tophat.Pred (Pred(..), pattern (:==:), pattern (:/\:))
import Tophat.Val (Val, asPred)

import qualified Data.Stream as Stream


ids :: Stream Nat
ids = Stream.iterate succ 1


type Simulation t = StateT Heap (RootT Text (SupplyT Nat Identity)) ( Val ('TyPrim t), List Input, Pred 'TyPrimBool )

runSimulation :: Simulation t -> ( Root Text ( ( Val ('TyPrim t), List Input, Pred 'TyPrimBool ), Heap ), Stream Nat )
runSimulation r = runIdentity (runSupplyT (runRootT (runStateT r empty)) ids)

execSimulation :: Simulation t -> Root Text ( Val ('TyPrim t), List Input, Pred 'TyPrimBool, Heap )
execSimulation = map go << fst << runSimulation
  where
    go ( ( v, is, p ), h ) = ( v, is, p, h )

evalSimulation :: Editable t => Simulation t -> List (Pred 'TyPrimBool)
evalSimulation = predicates << execSimulation

predicates :: Editable t => Root Text ( Val ('TyPrim t), List Input, Pred 'TyPrimBool, Heap ) -> List (Pred 'TyPrimBool)
predicates = map go << toList
  where
    go ( v, _, p, _ ) = (Sym 0 :==: asPred v) :/\: p



type Firsts = StateT Heap (RootT Text (SupplyT Nat Identity)) ( Input, Pred 'TyPrimBool )

runFirsts :: Firsts -> ( Root Text ( ( Input, Pred 'TyPrimBool ), Heap ), Stream Nat )
runFirsts r = runIdentity (runSupplyT (runRootT (runStateT r empty)) ids)

execFirsts :: Firsts -> Root Text ( Input, Pred 'TyPrimBool, Heap )
execFirsts = map go << fst << runFirsts
  where
    go ( ( v, p ), h ) = ( v, p, h )

evalFirsts :: Firsts -> List (Input, Pred 'TyPrimBool)
evalFirsts = predicatesFirsts << execFirsts

predicatesFirsts :: Root Text ( Input, Pred 'TyPrimBool, Heap ) -> List (Input, Pred 'TyPrimBool)
predicatesFirsts = map go << toList
  where
    go ( i, p, _ ) = (i,p) --(Sym 0 :==: asPred v) :/\: p
