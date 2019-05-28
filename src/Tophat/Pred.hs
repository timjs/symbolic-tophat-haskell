module Tophat.Pred
  ( module Tophat.Name
  , module Tophat.Type
  , Pred(..)
  , pattern Yes, pattern Nop, pattern Not, pattern (:/\:), pattern (:\/:)
  , simplify
  , toSat
  ) where

import Tophat.Type
import Tophat.Name

import Data.SBV

import qualified Tophat.Op as O


-- Predicates ------------------------------------------------------------------

data Pred (p :: PrimTy) where
  Con :: ( Editable p ) => TypeOf p -> Pred p
  Sym :: ( Editable p ) => Name ('TyPrim p) -> Pred p

  Un :: O.Un p q -> Pred p -> Pred q
  Bn :: O.Bn p q r -> Pred p -> Pred q -> Pred r


pattern Yes = Con True
pattern Nop = Con False

pattern Not x = Un O.Not x

pattern (:/\:) x y = Bn O.Conj x y
pattern (:\/:) x y = Bn O.Disj x y

instance Pretty (Pred t) where
  pretty = \case
    Con x -> pretty x
    Sym i -> "s" <> pretty i

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])


simplify :: Pred t -> Pred t
simplify = \case
  x :/\: y -> case ( simplify x, simplify y ) of
    ( Con True, p ) -> p
    ( p, Con True ) -> p
    ( p, q )        -> p :/\: q

  Un o x -> Un o (simplify x)
  Bn o x y -> Bn o (simplify x) (simplify y)

  p -> p


-- Conversions -----------------------------------------------------------------


toSat :: Pred t -> Symbolic (SBV (TypeOf t))
toSat = \case
  Con x -> pure $ literal x
  Sym i -> symbolic $ show (pretty i)

  Un o a -> O.toSatUn o <*> toSat a
  Bn o a b -> O.toSatBn o <*> toSat a <*> toSat b
