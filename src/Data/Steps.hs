module Data.Steps where


data Steps a
  = None
  | End a
  | Mid (Steps a) (Steps a)
  deriving ( Eq, Ord, Show, Read, Functor, Foldable, Traversable )


instance Pretty a => Pretty (Steps a) where
  pretty = \case
    None -> sep [ "-" ]
    End x -> sep [ "-", pretty x ]
    Mid ls rs -> split
      [ "+"
      , indent 2 $ pretty ls
      , indent 2 $ pretty rs
      ]


instance Applicative Steps where
  pure = End

  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')


instance Alternative Steps where
  empty = None

  ls <|> rs = Mid ls rs


instance Monad Steps where
  None      >>= _ = None
  End x     >>= k = k x
  Mid ls rs >>= k = Mid (ls >>= k) (rs >>= k)
