module Language.Expr.Sym where

import Control.Monad.Writer

import Data.SBV

import Language.Expr
import Language.Pred (Pred)
import qualified Language.Pred as Pred



-- Gathering ------------------------------------------------------------------


gather :: Env cxt -> Expr cxt sxt t -> List (Writer (List (Pred sxt 'TyBool)) (Expr cxt sxt t))
gather vars = \case

  -- Lam f -> \x -> gather (Cons x vars) f
  -- App f a -> gather vars f $ gather vars a
  -- Var i -> lookup i vars
  -- Sym _ -> error "Found Sym in executable expression" --FIXME: should be checkable
  -- Val i -> i

  Un o a -> lift1 (Un o) <$> gather vars a
  Bn o a b -> lift2 (Bn o) <$> gather vars a <*> gather vars b
  If p a b -> do
    p' <- gather vars p
    ( p'', _ ) <- runWriter p'
    _
    -- if gather vars p then gather vars a else gather vars b

  Unit -> pure $ pure Unit
  Pair a b -> lift2 Pair <$> gather vars a <*> gather vars b

  Fst e -> lift1 Fst <$> gather vars e
  Snd e -> lift1 Snd <$> gather vars e

--
