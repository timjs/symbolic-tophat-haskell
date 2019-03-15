module Data.Universe where



class Universe u where
  type TypeOf (a :: u)
  -- type TypeOf (a :: u) = r | r -> a
