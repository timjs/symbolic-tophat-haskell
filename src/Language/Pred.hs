module Language.Pred
  ( module Language.Types
  , Pred(..)
  , pattern Yes, pattern No, pattern Not, pattern (:/\:), pattern (:\/:)
  ) where

import Language.Types

import qualified Language.Ops as O


-- Predicates ------------------------------------------------------------------

data Pred (sxt :: List PrimTy) (t :: PrimTy) where
  Con :: IsPrim a -> ConcOf a -> Pred sxt a
  Sym :: HasType (t ': ts) a -> Pred (t ': ts) a

  Un :: O.Un a b -> Pred sxt a -> Pred sxt b
  Bn :: O.Bn a b c -> Pred sxt a -> Pred sxt b -> Pred sxt c

pattern Yes = Con BoolIsPrim True
pattern No  = Con BoolIsPrim False

pattern Not x = Un O.Not x

pattern (:/\:) x y = Bn O.Conj x y
pattern (:\/:) x y = Bn O.Disj x y
