module Data.Universe where



-- Universes -------------------------------------------------------------------


class Universe u where
  type TypeOf (a :: u) = c | c -> a
  type SymbOf (a :: u) = s | s -> a



-- Contexts --------------------------------------------------------------------


data HasType (cxt :: List u) (t :: u) where
  Here :: HasType (t ': ts) t
  There :: HasType ts t -> HasType (t2 ': ts) t


instance Pretty (HasType cxt t) where
  pretty = pretty << idx


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
