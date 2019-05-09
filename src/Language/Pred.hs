module Language.Pred
  ( module Language.Type
  , Pred(..)
  , pattern Yes, pattern Nop, pattern Not, pattern (:/\:), pattern (:\/:)
  ) where

import Language.Type
import Language.Name

import qualified Language.Op as O


-- Predicates ------------------------------------------------------------------

data Pred (t :: PrimTy) where
  Con :: IsPrim a -> TypeOf a -> Pred a
  Sym :: Name ('TyPrim a) -> Pred a

  Un :: O.Un a b -> Pred a -> Pred b
  Bn :: O.Bn a b c -> Pred a -> Pred b -> Pred c

pattern Yes = Con BoolIsPrim True
pattern Nop = Con BoolIsPrim False

pattern Not x = Un O.Not x

pattern (:/\:) x y = Bn O.Conj x y
pattern (:\/:) x y = Bn O.Disj x y

instance Pretty (Pred t) where
  pretty = \case
    Con BoolIsPrim x -> pretty x
    Con IntIsPrim x -> pretty x
    Con StringIsPrim x -> pretty x
    Sym i -> "s" <> pretty i

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])
