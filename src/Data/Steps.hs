module Data.Steps where

import Data.Something


data Steps a
  = Non (Something Pretty)
  | End (Something Pretty) a
  | Mid (Something Pretty) (Steps a) (Steps a)
  deriving ( Functor, Foldable, Traversable )


instance ( Pretty a ) => Pretty (Steps a) where
  pretty = \case
    Non (Some p) -> sep [ "x", pretty p ]
    End (Some p) x -> sep [ "-", pretty p, pretty x ]
    Mid (Some p) ls rs -> split
      [ sep [ "+", pretty p ]
      , indent 2 $ pretty ls
      , indent 2 $ pretty rs
      ]


record :: Something Pretty -> Steps a -> Steps a
record s = \case
  Non _ -> Non s
  End _ x -> End s x
  Mid _ ls rs -> Mid s ls rs


instance Applicative Steps where
  pure = End (Some ())

  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')


instance Alternative Steps where
  empty = Non (Some ())

  ls <|> rs = Mid (Some ()) ls rs  --XXX or combine?


instance Monad Steps where
  Non s      >>= _ = Non s
  End _ x     >>= k = k x --record (Some x) $ k x
  Mid s ls rs >>= k = Mid s (ls >>= k) (rs >>= k)
