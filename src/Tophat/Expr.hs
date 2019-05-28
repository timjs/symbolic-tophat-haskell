module Tophat.Expr
  ( module Tophat.Name
  , module Tophat.Type
  , Expr(..), Un(..), Bn(..)
  , pattern Let
  , Pretask(..)
  , pattern View, pattern Watch, pattern (:&&:), pattern (:||:), pattern (:??:), pattern (:>>=), pattern (:>>!), pattern (:>>?)
  , subst, subst', shift, shift'
  ) where

import Tophat.Name
import Tophat.Type
import Tophat.Op


-- Expressions -----------------------------------------------------------------

data Expr (t :: Ty) where
  -- | FIXME: Explain why Typeable.
  Lam :: ( Typeable a ) => Expr b -> Expr (a ':-> b)
  App :: ( Typeable a, Typeable b ) => Expr (a ':-> b) -> Expr a -> Expr b
  Var :: ( Typeable a ) => Name a -> Expr a

  Loc :: Name ('TyPrim p) -> Expr ('TyRef p)
  Sym :: ( Editable p ) => Name ('TyPrim p) -> Expr ('TyPrim p)
  Con :: ( Editable p ) => TypeOf p -> Expr ('TyPrim p)

  Un :: ( Typeable p, Typeable q ) => Un p q -> Expr ('TyPrim p) -> Expr ('TyPrim q)
  Bn :: ( Typeable p, Typeable q, Typeable r ) => Bn p q r -> Expr ('TyPrim p) -> Expr ('TyPrim q) -> Expr ('TyPrim r)
  If :: Expr ('TyPrim 'TyBool) -> Expr a -> Expr a -> Expr a

  Pair :: Expr a -> Expr b -> Expr (a ':>< b)
  Fst  :: ( Typeable a, Typeable b ) => Expr (a ':>< b) -> Expr a
  Snd  :: ( Typeable a, Typeable b ) => Expr (a ':>< b) -> Expr b

  Ref    :: ( Typeable p ) => Expr ('TyPrim p) -> Expr ('TyRef p)
  Deref  :: ( Typeable p ) => Expr ('TyRef p) -> Expr ('TyPrim p)
  Assign :: ( Typeable p ) => Expr ('TyRef p) -> Expr ('TyPrim p) -> Expr ('TyPrim 'TyUnit)

  Task :: Pretask ('TyTask a) -> Expr ('TyTask a)


pattern Let x b = App (Lam b) x


instance Pretty (Expr t) where
  pretty = \case
    Lam f -> cat [ "λ.", pretty f ]
    Let x b -> split [ sep [ "let", pretty x, "in" ], pretty b ]
    App f a -> sep [ parens (pretty f), parens (pretty a) ]
    Var i -> cat [ "x", pretty i ]
    Sym i -> cat [ "s", pretty i ]
    Loc i -> cat [ "l", pretty i ]

    Con x -> pretty x

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])
    If p a b -> sep [ "if", pretty p, "then", pretty a, "else", pretty b ]

    Pair a b -> angles $ cat [ pretty a, ",", pretty b ]
    Fst a -> sep [ "fst", pretty a ]
    Snd a -> sep [ "snd", pretty a ]

    Ref a -> sep [ "ref", pretty a ]
    Deref a -> cat [ "!", pretty a ]
    Assign a b -> sep [ pretty a, ":=", pretty b ]

    Task p -> pretty p


instance Eq (Expr t) where
  Lam f1                == Lam f2                = f1 == f2  -- FIXME: is this ok?
  App f1 a1             == App f2 a2
    | Just Refl <- f1 ~= f2                      = f1 == f2 && a1 == a2
    | otherwise                                  = False
  Var i1                == Var i2                = i1 == i2
  Sym i1                == Sym i2                = i1 == i2
  Loc i1                == Loc i2                = i1 == i2

  Con x1                == Con x2                = x1 == x2

  Un o1 a1              == Un o2 a2
    | Just Refl <- o1 ~= o2                      = o1 == o2 && a1 == a2
    | otherwise                                  = False
  Bn o1 a1 b1           == Bn o2 a2 b2
    | Just Refl <- o1 ~= o2                      = o1 == o2 && a1 == a2 && b1 == b2
    | otherwise                                  = False
  If p1 a1 b1           == If p2 a2 b2           = p1 == p2 && a1 == a2 && b1 == b2

  Pair a1 b1            == Pair a2 b2            = a1 == a2 && b1 == b2
  Fst a1                == Fst a2
    | Just Refl <- a1 ~= a2                      = a1 == a2
    | otherwise                                  = False
  Snd a1                == Snd a2
    | Just Refl <- a1 ~= a2                      = a1 == a2
    | otherwise                                  = False

  Ref a1                == Ref a2                = a1 == a2
  Deref a1              == Deref a2              = a1 == a2
  Assign a1 b1          == Assign a2 b2
    | Just Refl <- a1 ~= a2                      = a1 == a2 && b1 == b2
    | otherwise                                  = False

  Task p1               == Task p2               = p1 == p2

  _                     == _                     = False


-- Pretasks --------------------------------------------------------------------

data Pretask (t :: Ty) where
  Enter  :: ( Editable p ) => Pretask ('TyTask ('TyPrim p))
  Update :: ( Editable p ) => Expr ('TyPrim p) -> Pretask ('TyTask ('TyPrim p))
  Change :: ( Typeable p, Editable p ) => Expr ('TyRef p) -> Pretask ('TyTask ('TyPrim p))

  And  :: Expr ('TyTask a) -> Expr ('TyTask b) -> Pretask ('TyTask (a ':>< b))
  Or   :: Expr ('TyTask a) -> Expr ('TyTask a) -> Pretask ('TyTask a)
  Xor  :: Expr ('TyTask a) -> Expr ('TyTask a) -> Pretask ('TyTask a)
  Fail :: Pretask ('TyTask a)

  Then :: ( Typeable a, Typeable b ) => Expr ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Pretask ('TyTask b)
  Next :: ( Typeable a, Typeable b ) => Expr ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Pretask ('TyTask b)


infixl 3 :&&:
infixr 2 :||:, :??:
-- | NOTE:
-- | Fixity of bind is left associative in a normal setting because of the scoping of lambdas.
-- | Because we can't use lambdas in our DSL, bind should be right associative.
infixr 1 :>>=, :>>!, :>>?


pattern View x = Update x
pattern Watch x = Change x
pattern (:&&:) x y = And (Task x) (Task y)
pattern (:||:) x y = Or (Task x) (Task y)
pattern (:??:) x y = Xor (Task x) (Task y)
pattern (:>>=) t c = Then (Task t) (Lam (Task c))
pattern (:>>!) t c = Then (Task t) (Lam c)
pattern (:>>?) t c = Next (Task t) (Lam c)


instance Pretty (Pretask t) where
  pretty = \case
    Enter -> "⊠"
    Update x -> cat [ "□(", pretty x, ")" ]
    Change x -> cat [ "■(", pretty x, ")" ]
    And x y -> sep [ pretty x, "⋈", pretty y ]
    Or x y -> sep [ pretty x, "◆", pretty y ]
    Xor x y -> sep [ pretty x, "◇", pretty y ]
    Fail -> "↯"
    Then x c -> sep [ pretty x, "▶", pretty c ]
    Next x c -> sep [ pretty x, "▷", pretty c ]


instance Eq (Pretask t) where
  -- | This is where the magic happens!
  -- | Every editor with some symbol in it are regarded equal,
  -- | regardless of the concrete symbol they contain.
  Enter          == Enter          = True
  Update (Sym _) == Update (Sym _) = True
  Update x1      == Update x2      = x1 == x2
  Change x1      == Change x2      = x1 == x2

  And x1 y1      == And x2 y2      = x1 == x2 && y1 == y2
  Or x1 y1       == Or x2 y2       = x1 == x2 && y1 == y2
  Xor x1 y1      == Xor x2 y2      = x1 == x2 && y1 == y2
  Fail           == Fail           = True

  Then x1 c1     == Then x2 c2
    | Just Refl <- x1 ~= x2
    , Just Refl <- c1 ~= c2        = x1 == x2 && c1 == c2
    | otherwise                    = False
  Next x1 c1     == Next x2 c2
    | Just Refl <- x1 ~= x2
    , Just Refl <- c1 ~= c2        = x1 == x2 && c1 == c2
    | otherwise                    = False

  _              == _              = False


-- Substitution ----------------------------------------------------------------

-- | Substitution of a variable `x` with expression `v` in an expression `e`
subst
  :: Typeable s
  => Name s  -- ^ Substitute a name of type `s`
  -> Expr s  -- ^ with expression of type `s`
  -> Expr t  -- ^ in an expression of type `t` containing a variable of type `s`
  -> Expr t  -- ^ giving the modified expression of type `t`.
subst j s = \case
  Lam e -> go e
    where
      go :: forall a b. Typeable a => Expr b -> Expr (a ':-> b)
      go = Lam << subst (j + 1) (shift (0 :: Name a) s)  -- We need to tell Haskell that `0` is of type `Name a` here
  App f a -> App (subst j s f) (subst j s a)
  Var i
    | Just Refl <- i ~= j, i == j -> s
    | otherwise -> Var i

  Un o a -> Un o (subst j s a)
  Bn o a b -> Bn o (subst j s a) (subst j s b)
  If p a b -> If (subst j s p) (subst j s a) (subst j s b)
  Pair a b -> Pair (subst j s a) (subst j s b)
  Fst a -> Fst (subst j s a)
  Snd a -> Snd (subst j s a)
  Ref a -> Ref (subst j s a)
  Deref a -> Deref (subst j s a)
  Assign a b -> Assign (subst j s a) (subst j s b)
  Task p -> Task (subst' j s p)

  Loc i -> Loc i
  Sym i -> Sym i
  Con x -> Con x


-- | Same for pretasks `p`.
subst' :: Typeable s => Name s -> Expr s -> Pretask t -> Pretask t
subst' j s = \case
  Enter -> Enter
  Update a -> Update (subst j s a)
  Change a -> Change (subst j s a)
  And a b -> And (subst j s a) (subst j s b)
  Or a b -> Or (subst j s a) (subst j s b)
  Xor a b -> Xor (subst j s a) (subst j s b)
  Fail -> Fail
  Then a b -> Then (subst j s a) (subst j s b)
  Next a b -> Next (subst j s a) (subst j s b)


-- | The one-place shift of an expression `e` after cutof `c`.
shift :: Typeable s => Name s -> Expr t -> Expr t
shift c = \case
  Lam e -> Lam $ shift (c + 1) e
  App f a -> App (shift c f) (shift c a)
  Var i
    | Just Refl <- i ~= c, i >= c -> Var (i + 1)
    | otherwise -> Var i

  Un o a -> Un o (shift c a)
  Bn o a b -> Bn o (shift c a) (shift c b)
  If p a b -> If (shift c p) (shift c a) (shift c b)
  Pair a b -> Pair (shift c a) (shift c b)
  Fst a -> Fst (shift c a)
  Snd a -> Snd (shift c a)
  Ref a -> Ref (shift c a)
  Deref a -> Deref (shift c a)
  Assign a b -> Assign (shift c a) (shift c b)
  Task p -> Task (shift' c p)

  Loc i -> Loc i
  Sym i -> Sym i
  Con x -> Con x


-- | Same for pretasks `p`.
shift' :: Typeable s => Name s -> Pretask t -> Pretask t
shift' c = \case
  Enter -> Enter
  Update a -> Update (shift c a)
  Change a -> Change (shift c a)
  And a b -> And (shift c a) (shift c b)
  Or a b -> Or (shift c a) (shift c b)
  Xor a b -> Xor (shift c a) (shift c b)
  Fail -> Fail
  Then a b -> Then (shift c a) (shift c b)
  Next a b -> Next (shift c a) (shift c b)
