module Data.Steps where

import Data.Something


data Steps a
  = None (Something Pretty)
  | End (Something Pretty) a
  | Mid (Something Pretty) (Steps a) (Steps a)
  deriving ( Functor, Foldable, Traversable )


record :: Something Pretty -> Steps a -> Steps a
record h = \case
  None _ -> None h
  End _ a -> End h a
  Mid _ ls rs -> Mid h ls rs


instance ( Pretty a ) => Pretty (Steps a) where
  pretty = \case
    None (Some h) -> sep [ "x", pretty h ]
    End (Some h) x -> sep [ "-", pretty h, pretty x ]
    Mid (Some h) ls rs -> split
      [ sep [ "+", pretty h ]
      , indent 2 $ pretty ls
      , indent 2 $ pretty rs
      ]


--


instance Applicative Steps where
  pure = End (Some ())

  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')


instance Alternative Steps where
  empty = None (Some ())

  ls <|> rs = Mid (Some ()) ls rs  --XXX or combine?


instance Monad Steps where
  None h      >>= _ = None h
  End _ x     >>= k = k x --record (Some x) $ k x
  Mid h ls rs >>= k = Mid h (ls >>= k) (rs >>= k)
