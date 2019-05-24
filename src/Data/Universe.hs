module Data.Universe
  ( Universe(..)
  , HasType(..), idx
  , Env(..), lookup
  ) where


-- Universes -------------------------------------------------------------------

class Universe u where
  type TypeOf (a :: u)


-- Contexts --------------------------------------------------------------------

data HasType (cxt :: List u) (t :: u) where
  Here :: HasType (a ': cxt) a
  There :: HasType cxt a -> HasType (b ': cxt) a


instance Pretty (HasType cxt t) where
  pretty = pretty << idx

instance Eq (HasType cxt t) where
  Here    == Here    = True
  There i == There j = i == j
  _       == _       = False

instance Ord (HasType cxt t) where
  compare Here Here = EQ
  compare Here (There _) = LT
  compare (There _) Here = GT
  compare (There i) (There j) = compare i j


idx :: HasType cxt t -> Int
idx = \case
  Here -> 0
  There xs -> 1 + idx xs


-- Environments ----------------------------------------------------------------

data Env (cxt :: List u) where
  Nil :: Env '[]
  Cons :: TypeOf t -> Env ts -> Env (t ': ts)


lookup :: HasType cxt t -> Env cxt -> TypeOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs
