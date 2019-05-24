module Tophat.Val
  ( module Tophat.Type
  , module Tophat.Name
  , Val(..), Un(..), Bn(..)
  , pattern U, pattern B, pattern I, pattern S
  , Task(..)
  , pattern View, pattern (:&&:), pattern (:||:), pattern (:??:), pattern (:>>=), pattern (:>>\), pattern (:>>?)
  , asPred, asExpr, asPretask
  ) where

-- | NOTE:
-- | If we were programming in a dependently typed language,
-- | We could make a predicate `IsVal` over `Expr`.
-- | Alas, here we use a separate data type to keep values and expressions
-- | distinct.

import Tophat.Type
import Tophat.Name
import Tophat.Op

import Tophat.Expr (Expr)

import qualified Tophat.Expr as E
import qualified Tophat.Pred as P


-- Expressions -----------------------------------------------------------------

data Val (t :: Ty) where
  Lam :: Typeable a => Expr b -> Val (a ':-> b)

  Loc :: Name ('TyPrim p) -> Val ('TyRef p)
  Sym :: Name ('TyPrim p) -> Val ('TyPrim p)
  Con :: IsPrim p -> TypeOf p -> Val ('TyPrim p)

  Un :: ( Typeable p, Typeable q ) => Un p q -> Val ('TyPrim p) -> Val ('TyPrim q)
  Bn :: ( Typeable p, Typeable q, Typeable r ) => Bn p q r -> Val ('TyPrim p) -> Val ('TyPrim q) -> Val ('TyPrim r)

  Pair :: Val a -> Val b -> Val (a ':>< b)

  Task :: Task ('TyTask a) -> Val ('TyTask a)


pattern U   = Con UnitIsPrim ()
pattern B x = Con BoolIsPrim x
pattern I x = Con IntIsPrim x
pattern S x = Con StringIsPrim x


instance Pretty (Val t) where
  pretty = \case
    Lam f -> cat [ "λ.", pretty f ]
    Sym i -> cat [ "s", pretty i ]
    Loc i -> cat [ "l", pretty i ]

    Con UnitIsPrim x -> pretty x
    Con BoolIsPrim x -> pretty x
    Con IntIsPrim x -> pretty x
    Con StringIsPrim x -> cat [ "\"", pretty x, "\"" ]

    Un o a -> parens $ sep [ pretty o, pretty a ]
    Bn o a b -> parens $ sep [ pretty a, pretty o, pretty b ]

    Pair a b -> angles $ cat [ pretty a, ",", pretty b ]

    Task p -> pretty p


-- | Syntactic equality for Values.
instance Eq (Val t) where
  Lam f1                == Lam f2                = f1 == f2  -- FIXME: is this ok?
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

  Pair a1 b1            == Pair a2 b2            = a1 == a2 && b1 == b2

  Task p1               == Task p2               = p1 == p2

  _                     == _                     = False


-- Tasks -----------------------------------------------------------------------

data Task (t :: Ty) where
  Edit :: Val ('TyPrim p) -> Task ('TyTask ('TyPrim p))
  Enter :: Task ('TyTask ('TyPrim p))
  Update :: Typeable p => Val ('TyRef p) -> Task ('TyTask ('TyPrim p))

  And :: Val ('TyTask a) -> Val ('TyTask b) -> Task ('TyTask (a ':>< b))
  Or  :: Val ('TyTask a) -> Val ('TyTask a) -> Task ('TyTask a)
  Xor :: Expr ('TyTask a) -> Expr ('TyTask a) -> Task ('TyTask a)
  Fail :: Task ('TyTask a)

  -- | `a` and `b` should be typable for testing syntactic equality of terms.
  Then :: ( Typeable a, Typeable b ) => Val ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Task ('TyTask b)
  Next :: ( Typeable a, Typeable b ) => Val ('TyTask a) -> Expr (a ':-> 'TyTask b) -> Task ('TyTask b)


infixl 3 :&&:
infixr 2 :||:, :??:
-- | NOTE:
-- | Fixity of bind is left associative in a normal setting because of the scoping of lambdas.
-- | Because we can't use lambdas in our DSL, bind should be right associative.
infixr 1 :>>=, :>>?, :>>\


pattern View x = Edit x
pattern (:&&:) x y = And (Task x) (Task y)
pattern (:||:) x y = Or (Task x) (Task y)
pattern (:??:) x y = Xor (E.Task x) (E.Task y)
pattern (:>>=) t c = Then (Task t) (E.Lam (E.Task c))
pattern (:>>\) t c = Then (Task t) (E.Lam c)
pattern (:>>?) t c = Next (Task t) (E.Lam (E.Task c))


instance Pretty (Task t) where
  pretty = \case
    Edit x -> cat [ "□(", pretty x, ")" ]
    Enter -> "⊠"
    Update x -> cat [ "■(", pretty x, ")" ]
    And x y -> sep [ pretty x, "⋈", pretty y ]
    Or x y -> sep [ pretty x, "◆", pretty y ]
    Xor x y -> sep [ pretty x, "◇", pretty y ]
    Fail -> "↯"
    Then x c -> sep [ pretty x, "▶", pretty c ]
    Next x c -> sep [ pretty x, "▷", pretty c ]


-- | Syntactic equality for Tasks.
instance Eq (Task t) where
  -- | This is where the magic happens!
  -- | Every editor with some symbol in it are regarded equal,
  -- | regardless of the concrete symbol they contain.
  Edit (Sym _) == Edit (Sym _) = True
  Edit x1      == Edit x2      = x1 == x2
  Enter        == Enter        = True
  Update x1    == Update x2    = x1 == x2

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


-- Translation -----------------------------------------------------------------

asPred :: Val ('TyPrim a) -> P.Pred a
asPred = \case
  Sym i -> P.Sym i
  Con p x -> P.Con p x

  Un o v1 -> P.Un o (asPred v1)
  Bn o v1 v2 -> P.Bn o (asPred v1) (asPred v2)


asExpr :: Val a -> Expr a
asExpr = \case
  Lam f -> E.Lam f
  Sym i -> E.Sym i
  Loc i -> E.Loc i
  Con p x -> E.Con p x

  Un o a -> E.Un o (asExpr a)
  Bn o a b -> E.Bn o (asExpr a) (asExpr b)

  Pair a b -> E.Pair (asExpr a) (asExpr b)

  Task p -> E.Task (asPretask p)


asPretask :: Task a -> E.Pretask a
asPretask = \case
  Edit x -> E.Edit (asExpr x)
  Enter -> E.Enter
  Update x -> E.Update (asExpr x)
  And x y -> E.And (asExpr x) (asExpr y)
  Or x y -> E.Or (asExpr x) (asExpr y)
  Xor x y -> E.Xor x y
  Fail -> E.Fail
  Then x c -> E.Then (asExpr x) c
  Next x c -> E.Next (asExpr x) c
