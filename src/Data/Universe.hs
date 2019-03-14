module Data.Universe where



class Universe u where
  type TypeOf (a :: u)
  -- type SymOf (a :: u)
