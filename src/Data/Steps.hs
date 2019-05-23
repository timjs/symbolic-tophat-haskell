module Data.Steps where


data Steps t a
  = Non t
  | End t a
  | Mid t (Steps t a) (Steps t a)
  deriving ( Eq, Ord, Show, Read, Functor, Foldable, Traversable )


instance ( Pretty t, Pretty a ) => Pretty (Steps t a) where
  pretty = \case
    Non t -> sep [ "x", pretty t ]
    End t x -> sep [ "-", pretty t, pretty x ]
    Mid t ls rs -> split
      [ sep [ "+", pretty t ]
      , indent 2 $ pretty ls
      , indent 2 $ pretty rs
      ]


save :: Monoid t => t -> Steps t a -> Steps t a
save t = \case
  Non s -> Non (s <> t)
  End s x -> End (s <> t) x
  Mid s ls rs -> Mid (s <> t) ls rs


instance ( Monoid t ) => Applicative (Steps t) where
  pure = End neutral --(traceShow "Data.Steps.pure: created neutral" neutral)

  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')


instance ( Monoid t ) => Alternative (Steps t) where
  empty = Non neutral --(traceShow "Data.Steps.empty: created neutral" neutral)

  -- ls <|> rs = Mid neutral ls rs

  -- Non _ <|> rs    = rs
  -- ls    <|> Non _ = ls
  -- ls    <|> rs    = Mid neutral ls rs

  Non t1       <|> Non t2       = Non (t1 <> t2)
  Non t1       <|> End t2 x2    = End (t1 <> t2) x2
  Non t1       <|> Mid t2 l2 r2 = Mid (t1 <> t2) l2 r2
  End t1 x1    <|> Non t2       = End (t1 <> t2) x1
  Mid t1 l1 r2 <|> Non t2       = Mid (t1 <> t2) l1 r2
  ls           <|> rs           = Mid neutral ls rs --(traceShow "Data.Steps.<|>: created neutral" neutral) ls rs


instance ( Monoid t ) => Monad (Steps t) where
  Non t       >>= _ = Non t
  End t x     >>= k = save t $ k x
  Mid t ls rs >>= k = Mid t (ls >>= k) (rs >>= k)
