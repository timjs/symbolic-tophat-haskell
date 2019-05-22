module Data.Steps where


data Steps h a
  = None h
  | End h a
  | Mid h (Steps h a) (Steps h a)
  deriving ( Eq, Ord, Show, Read, Functor, Foldable, Traversable )


record :: Monoid h => h -> Steps h a -> Steps h a
record h = \case
  None g -> None (g <> h)
  End g a -> End (g <> h) a
  Mid g ls rs -> Mid (g <> h) ls rs


instance ( Pretty h, Pretty a ) => Pretty (Steps h a) where
  pretty = \case
    None h -> sep [ "[x]", pretty h ]
    End h x -> sep [ "[-]", pretty h, pretty x ]
    Mid h ls rs -> split
      [ sep [ "[+]", pretty h ]
      , indent 2 $ pretty ls
      , indent 2 $ pretty rs
      ]


instance Monoid h => Applicative (Steps h) where
  pure = End neutral

  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')


instance Monoid h => Alternative (Steps h) where
  empty = None neutral

  ls <|> rs = Mid neutral ls rs  --XXX or combine?


instance Monoid h => Monad (Steps h) where
  None h      >>= _ = None h
  End h x     >>= k = record h $ k x
  Mid h ls rs >>= k = Mid h (ls >>= k) (rs >>= k)
