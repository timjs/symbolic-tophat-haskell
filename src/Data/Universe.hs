module Data.Universe where



-- Universes -------------------------------------------------------------------


data {- kind -} Sort
  = Symbolic
  | Concrete


class Universe u where
  type TypeOf (s :: Sort) (a :: u)


type ConcOf a = TypeOf 'Concrete a
type SymbOf a = TypeOf 'Symbolic a



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


data Env (s :: Sort) (cxt :: List u) where
  Nil :: Env s '[]
  Cons :: TypeOf s t -> Env s ts -> Env s (t ': ts)


lookup :: HasType cxt t -> Env s cxt -> TypeOf s t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs


type ConcEnv cxt = Env 'Concrete cxt
type SymbEnv cxt = Env 'Symbolic cxt
