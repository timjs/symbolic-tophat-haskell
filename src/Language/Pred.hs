module Language.Pred
  ( Pred(..)
  ) where


import Language.Types
import Language.Ops



-- Predicates ------------------------------------------------------------------


data Pred (sxt :: List Ty) (t :: Ty) where
  Con :: IsPrim a -> ConcOf a -> Pred sxt a
  Sym :: HasType (t ': ts) a -> Pred (t ': ts) a

  Un :: Un a b -> Pred sxt a -> Pred sxt b
  Bn :: Bn a b c -> Pred sxt a -> Pred sxt b -> Pred sxt c
