module Tophat.Pred
  ( module Tophat.Name
  , module Tophat.Type
  , Pred(..)
  , pattern Yes, pattern Nop, pattern Not, pattern (:/\:), pattern (:\/:)
  , simplify
  , toSmt, satisfiable
  ) where

import Tophat.Type
import Tophat.Name

import Data.SBV (SBV, symbolic, literal, isSatisfiable, kindOf, Kind)
import Data.SBV.Dynamic (Symbolic, SVal)
import Data.Some (Any, Some, pack, unsafeUnpack)
import Data.Something (Something(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.SBV.Dynamic as Smt
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Tophat.Op as O


-- Predicates ------------------------------------------------------------------

data Pred (p :: PrimTy) where
  Con :: ( Editable p ) => TypeOf p -> Pred p
  Sym :: ( Editable p ) => Name ('TyPrim p) -> Pred p

  Un :: ( Typeable p, Typeable q ) => O.Un p q -> Pred p -> Pred q
  Bn :: ( Typeable p, Typeable q, Typeable r ) => O.Bn p q r -> Pred p -> Pred q -> Pred r


pattern Yes = Con True
pattern Nop = Con False

pattern Not x = Un O.Not x

pattern (:/\:) x y = Bn O.Conj x y
pattern (:\/:) x y = Bn O.Disj x y

instance Pretty (Pred t) where
  pretty = \case
    Con x -> pretty x
    Sym i -> "s" <> pretty i

    Un o a -> parens (sep [ pretty o, pretty a ])
    Bn o a b -> parens (sep [ pretty a, pretty o, pretty b ])


simplify :: Pred t -> Pred t
simplify = \case
  x :/\: y -> case ( simplify x, simplify y ) of
    ( Con True, p ) -> p
    ( p, Con True ) -> p
    ( p, q )        -> p :/\: q

  Un o x -> Un o (simplify x)
  Bn o x y -> Bn o (simplify x) (simplify y)

  p -> p


-- Satisfiability --------------------------------------------------------------

toSmt :: Pred t -> Symbolic (SBV (TypeOf t))
toSmt = \case
  Con x -> pure $ literal x
  Sym i -> symbolic $ show (pretty i)

  Un o a -> O.toSmtUn o <*> toSmt a
  Bn o a b -> O.toSmtBn o <*> toSmt a <*> toSmt b


gatherFree :: forall t. Pred t -> Set ( Nat, Kind )
gatherFree = \case
  Con _ -> neutral
  Sym (Name n) -> Set.singleton ( n, kindOf @(TypeOf t) undefined )

  Un _ a -> gatherFree a
  Bn _ a b -> gatherFree a <> gatherFree b

{-
makeFree :: Set (Any Name) -> Symbolic (Map Nat SVal)
makeFree ns = let
  names = toList ns
  pairs = traverse createFree names
  in
    _
  where

  -- let
  -- nums = Set.toList $ map (\name -> unname << unsafeUnpack) names
  -- vars = traverse createFree nums
  -- in pure (zip nums) <*> vars
  -- where

-- createFree :: Smt.SymVal (TypeOf a) => Name a -> Symbolic ( Nat, SBV (TypeOf a) )
-- createFree (Name n) = pure ( n, ) <*> Smt.free ("s" <> show n)

createFree :: Any Name -> Symbolic ( Nat, SVal )
createFree aname = do
  let name = unsafeUnpack aname
  state <- ask
  var <- liftIO $ Smt.svMkSymVar Nothing (kindOf name) (Just $ show $ pretty name) state
  pure ( unname name, var )
-}

satisfiable :: Pred 'TyBool -> Bool
satisfiable p = unsafePerformIO do
  r <- isSatisfiable (toSmt p)
  pure $ tracePretty r
