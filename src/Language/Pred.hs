module Language.Pred
  ( module Language.Types
  , Pred(..)
  , pattern Yes, pattern No, pattern Not, pattern (:/\:), pattern (:\/:)
  ) where

import Language.Types
import Language.Names

import qualified Language.Ops as O


-- Predicates ------------------------------------------------------------------

data Pred (t :: PrimTy) where
  Con :: IsPrim a -> ConcOf a -> Pred a
  Sym :: Name ('TyPrim a) -> Pred a

  Un :: O.Un a b -> Pred a -> Pred b
  Bn :: O.Bn a b c -> Pred a -> Pred b -> Pred c

pattern Yes = Con BoolIsPrim True
pattern No  = Con BoolIsPrim False

pattern Not x = Un O.Not x

pattern (:/\:) x y = Bn O.Conj x y
pattern (:\/:) x y = Bn O.Disj x y
