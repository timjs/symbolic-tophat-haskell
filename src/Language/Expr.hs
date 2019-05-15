module Language.Expr
  ( module Language.Type
  , module Language.Name
  , Expr(..), Un(..), Bn(..)
  , pattern B, pattern I, pattern S
  , Pretask(..)
  , pattern View, pattern (:&&:), pattern (:||:), pattern (:??:), pattern (:>>=), pattern (:>>\), pattern (:>>?)
  ) where


import Language.Name
import Language.Type
import Language.Op



-- Expressions -----------------------------------------------------------------


data Expr (t :: Ty) where
  -- | FIXME: Explain why Typeable.
  Lam :: Typeable a => Expr b -> Expr (a ':-> b)
  App :: ( Typeable a, Typeable b ) => Expr (a ':-> b) -> Expr a -> Expr b
  Var :: Typeable a => Name a -> Expr a

  Loc :: Name ('TyPrim a) -> Expr ('TyRef a)
  Sym :: Name ('TyPrim a) -> Expr ('TyPrim a)
  Con :: IsPrim a -> TypeOf a -> Expr ('TyPrim a)

  Un :: ( Typeable a, Typeable b ) => Un a b -> Expr ('TyPrim a) -> Expr ('TyPrim b)
  Bn :: ( Typeable a, Typeable b, Typeable c ) => Bn a b c -> Expr ('TyPrim a) -> Expr ('TyPrim b) -> Expr ('TyPrim c)
  If :: Expr ('TyPrim 'TyBool) -> Expr a -> Expr a -> Expr a

  Unit :: Expr 'TyUnit
  Pair :: Expr a -> Expr b -> Expr (a ':>< b)
  Fst :: ( Typeable a, Typeable b ) => Expr (a ':>< b) -> Expr a
  Snd :: ( Typeable a, Typeable b ) => Expr (a ':>< b) -> Expr b

  Ref :: Expr ('TyPrim a) -> Expr ('TyRef a)
  Deref :: Expr ('TyRef a) -> Expr ('TyPrim a)
  Assign :: Typeable a => Expr ('TyRef a) -> Expr ('TyPrim a) -> Expr ('TyUnit)

  Task :: Pretask ('TyTask a) -> Expr ('TyTask a)


pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Expr t) where
  pretty = \case
    Lam f -> cat [ "λ.", pretty f ]
    App f a -> sep [ parens (pretty f), parens (pretty a) ]
    Var i -> cat [ "x", pretty i ]
    Sym i -> cat [ "s", pretty i ]
    Loc i -> cat [ "l", pretty i ]

    Con BoolIsPrim x -> pretty x
    Con IntIsPrim x -> pretty x
    Con StringIsPrim x -> pretty x

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])
    If p a b -> sep [ "if", pretty p, "then", pretty a, "else", pretty b ]

    Unit -> angles neutral
    Pair a b -> angles $ cat [ pretty a, comma, pretty b ]
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

  Con BoolIsPrim x1     == Con BoolIsPrim x2     = x1 == x2
  Con IntIsPrim x1      == Con IntIsPrim x2      = x1 == x2
  Con StringIsPrim x1   == Con StringIsPrim x2   = x1 == x2

  Un o1 a1              == Un o2 a2
    | Just Refl <- o1 ~= o2                      = o1 == o2 && a1 == a2
    | otherwise                                  = False
  Bn o1 a1 b1           == Bn o2 a2 b2
    | Just Refl <- o1 ~= o2                      = o1 == o2 && a1 == a2 && b1 == b2
    | otherwise                                  = False
  If p1 a1 b1           == If p2 a2 b2           = p1 == p2 && a1 == a2 && b1 == b2

  Unit                  == Unit                  = True
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
  Edit :: Expr ('TyPrim a) -> Pretask ('TyTask ('TyPrim a))
  Enter :: Pretask ('TyTask ('TyPrim a))
  -- Store :: Loc a -> Pretask ('TyTask a)

  And :: Expr ('TyTask a) -> Expr ('TyTask b) -> Pretask ('TyTask (a ':>< b))
  Or  :: Expr ('TyTask a) -> Expr ('TyTask a) -> Pretask ('TyTask a)
  Xor :: Expr ('TyTask a) -> Expr ('TyTask a) -> Pretask ('TyTask a)
  Fail :: Pretask ('TyTask a)

  Then :: ( Typeable a, Typeable b ) => Expr ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Pretask ('TyTask b)
  Next :: ( Typeable a, Typeable b ) => Expr ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Pretask ('TyTask b)


infixl 3 :&&:
infixr 2 :||:, :??:
-- | NOTE:
-- | Fixity of bind is left associative in a normal setting because of the scoping of lambdas.
-- | Because we can't use lambdas in our DSL, bind should be right associative.
infixr 1 :>>=, :>>?, :>>\


pattern View x = Edit x
pattern (:&&:) x y = And (Task x) (Task y)
pattern (:||:) x y = Or (Task x) (Task y)
pattern (:??:) x y = Xor (Task x) (Task y)
pattern (:>>=) t c = Then (Task t) (Lam (Task c))
pattern (:>>\) t c = Then (Task t) (Lam c)
pattern (:>>?) t c = Next (Task t) (Lam (Task c))


instance Pretty (Pretask t) where
  pretty = \case
    Edit x -> cat [ "□(", pretty x, ")" ]
    Enter -> "⊠"
    -- Store -> "■(_)"
    And x y -> sep [ pretty x, "⋈", pretty y ]
    Or x y -> sep [ pretty x, "◆", pretty y ]
    Xor x y -> sep [ pretty x, "◇", pretty y ]
    Fail -> "↯"
    Then x c -> sep [ pretty x, "▶", pretty c ]
    Next x c -> sep [ pretty x, "▷…", pretty c ]



instance Eq (Pretask t) where
  -- | This is where the magic happens!
  -- | Every editor with some symbol in it are regarded equal,
  -- | regardless of the concrete symbol they contain.
  Edit (Sym _) == Edit (Sym _) = True
  Edit x1      == Edit x2      = x1 == x2
  Enter        == Enter        = True
  -- Store     == -- Store     = _

  And x1 y1    == And x2 y2    = x1 == x2 && y1 == y2
  Or x1 y1     == Or x2 y2     = x1 == x2 && y1 == y2
  Xor x1 y1    == Xor x2 y2    = x1 == x2 && y1 == y2
  Fail         == Fail         = True

  Then x1 c1   == Then x2 c2
    | Just Refl <- x1 ~= x2
    , Just Refl <- c1 ~= c2    = x1 == x2 && c1 == c2
    | otherwise                = False
  Next x1 c1   == Next x2 c2
    | Just Refl <- x1 ~= x2
    , Just Refl <- c1 ~= c2    = x1 == x2 && c1 == c2
    | otherwise                = False

  _            == _            = False
