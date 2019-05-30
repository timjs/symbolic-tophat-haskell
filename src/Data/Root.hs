module Data.Root
  ( Root(..)
  , save, clear
  ) where


-- | A binary tree with its data at the roots and general information in the nodes.
-- |
-- | Note: the order of the constructors matters for performance (see Data.IntMap.Internal).
data Root v a
  = Bin v (Root v a) (Root v a)
  | Tip v a
  | Nil v
  deriving ( Eq, Ord, Show, Read, Functor, Foldable, Traversable )


instance ( Pretty v, Pretty a ) => Pretty (Root v a) where
  pretty = split << helper
    where
      helper :: Root v a -> List (Doc n)
      helper = \case
        Bin v ls rs -> pretty v : sub ls rs
        Tip v x -> [ pretty v <> ": " <> pretty x ]
        Nil v -> [ pretty v <> ": ╳" ]

      sub :: Root v a -> Root v a -> List (Doc n)
      sub ls rs = pad "├ " "│ " (helper rs) <> pad "└ " "  " (helper ls)

      pad :: Doc n -> Doc n -> List (Doc n) -> List (Doc n)
      pad x y = zipWith (<>) (x : repeat y)


-- | Save information 'v' in the current node.
save :: Monoid v => v -> Root v a -> Root v a
save v = \case
  Bin w ls rs -> Bin (v <> w) ls rs
  Tip w x -> Tip (v <> w) x
  Nil w -> Nil (v <> w)


clear :: Root v a -> Root () a
clear = \case
  Bin _ ls rs -> Bin () (clear ls) (clear rs)
  Tip _ x -> Tip () x
  Nil _ -> Nil ()


instance ( Monoid v ) => Applicative (Root v) where
  pure = Tip neutral --(traceShow "Data.Root.pure: created neutral" neutral)

  f <*> x = do
    f' <- f
    x' <- x
    pure (f' x')


instance ( Monoid v ) => Alternative (Root v) where
  empty = Nil neutral --(traceShow "Data.Root.empty: created neutral" neutral)

  ls <|> rs = Bin neutral ls rs

  -- Nil _ <|> rs    = rs
  -- ls    <|> Nil _ = ls
  -- ls    <|> rs    = Bin neutral ls rs

  -- Nil v1       <|> Nil v2       = Nil (v1 <> v2)
  -- Nil v1       <|> Tip v2 x2    = Tip (v1 <> v2) x2
  -- Nil v1       <|> Bin v2 l2 r2 = Bin (v1 <> v2) l2 r2
  -- Tip v1 x1    <|> Nil v2       = Tip (v1 <> v2) x1
  -- Bin v1 l1 r2 <|> Nil v2       = Bin (v1 <> v2) l1 r2
  -- ls           <|> rs           = Bin neutral ls rs --(traceShow "Data.Root.<|>: created neutral" neutral) ls rs


instance ( Monoid v ) => Monad (Root v) where
  Bin v ls rs >>= k = Bin v (ls >>= k) (rs >>= k)
  Tip v x     >>= k = save v $ k x
  Nil v       >>= _ = Nil v
