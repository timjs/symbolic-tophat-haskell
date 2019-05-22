module Language.Pred
  ( module Language.Type
  , Pred(..)
  , pattern Yes, pattern Nop, pattern Not, pattern (:/\:), pattern (:\/:), pattern B, pattern I, pattern S
  , simplify
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

pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Pred t) where
  pretty = \case
    Con UnitIsPrim x -> pretty x
    Con BoolIsPrim x -> pretty x
    Con IntIsPrim x -> pretty x
    Con StringIsPrim x -> pretty x
    Sym i -> "s" <> pretty i

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])


simplify :: Pred t -> Pred t
simplify = \case
  x :/\: y -> case ( simplify x, simplify y ) of
    ( B True, p ) -> p
    ( p, B True ) -> p
    ( p, q )      -> p :/\: q

  Un o x -> Un o (simplify x)
  Bn o x y -> Bn o (simplify x) (simplify y)

  p -> p
