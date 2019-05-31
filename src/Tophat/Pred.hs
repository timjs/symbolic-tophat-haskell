module Tophat.Pred
  ( module Tophat.Name
  , module Tophat.Type
  , Pred(..)
  , pattern Yes, pattern Nop, pattern Not, pattern (:/\:), pattern (:\/:), pattern (:==:)
  , simplify
  , toSmt, satisfiable
  ) where

import Tophat.Type
import Tophat.Name

import Data.SBV
import Data.SBV.Internals (SBV(..), unSBV, SVal)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.SBV.Dynamic as Smt
import qualified Data.SBV.List as Smt
import qualified Data.SBV.Tuple as Smt
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Tophat.Op as O


-- Predicates ------------------------------------------------------------------

data Pred (p :: PrimTy) where
  Con :: ( Editable p ) => TypeOf p -> Pred p
  Sym :: ( Editable p ) => Name ('TyPrim p) -> Pred p

  Pair :: ( Editable p, Editable q ) => Pred p -> Pred q -> Pred ('TyPrimPair p q)

  Nil  :: ( Editable p ) => Pred ('TyPrimList p)
  Cons :: ( Editable p ) => Pred p -> Pred ('TyPrimList p) -> Pred ('TyPrimList p)

  Un :: ( Typeable p, Typeable q ) => O.Un p q -> Pred p -> Pred q
  Bn :: ( Typeable p, Typeable q, Typeable r ) => O.Bn p q r -> Pred p -> Pred q -> Pred r


pattern Yes = Con True
pattern Nop = Con False

pattern Not x = Un O.Not x

pattern (:/\:) x y = Bn O.Conj x y
pattern (:\/:) x y = Bn O.Disj x y

pattern (:==:) x y = Bn O.Eq x y


instance Pretty (Pred t) where
  pretty = \case
    Con x -> pretty x
    Sym i -> cat [ "s", pretty i ]

    Pair a b -> angles $ cat [ pretty a, ", ", pretty b ]

    Nil -> "[]"
    -- Nil -> cat [ "[]_", pretty (typeRep @t) ]
    Cons a as -> sep [ pretty a, "::", pretty as ]

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

  Not (Con True) -> Con False
  Not (Con False) -> Con True

  p -> p


-- Satisfiability --------------------------------------------------------------


toSmt :: forall t. Map Nat SVal -> Pred t -> SVal
toSmt ss = \case
  Con x -> unSBV $ literal x
  Sym (Name n) ->
    fromMaybe (error $ "Tophat.Pred.toSmt: could not find symbol " <> show n <> " in table " <> show ss) $
    Map.lookup n ss

  Pair a b -> svPair (Proxy :: Proxy t) (toSmt ss a) (toSmt ss b)

  Nil -> svNil (Proxy :: Proxy t)
  Cons a as -> svCons (Proxy :: Proxy t) (toSmt ss a) (toSmt ss as)

  Un o a -> O.toSmtUn o (toSmt ss a)
  Bn o a b -> O.toSmtBn o (toSmt ss a) (toSmt ss b)


gatherFree :: forall t. Pred t -> Set ( Nat, Kind )
gatherFree = \case
  Con _ -> neutral
  Sym (Name n) -> Set.singleton ( n, kindOf (Proxy @(TypeOf t)) )

  Pair a b -> gatherFree a <> gatherFree b

  Nil -> neutral
  Cons a as -> gatherFree a <> gatherFree as

  Un _ a -> gatherFree a
  Bn _ a b -> gatherFree a <> gatherFree b


createFree :: Nat -> Kind -> Symbolic ( Nat, SVal )
createFree n k = do
  s <- symbolicEnv
  x <- liftIO $ Smt.svMkSymVar Nothing k (Just $ "s" <> show n) s
  pure ( n, x )


makeMap :: Pred t -> Symbolic (Map Nat SVal)
makeMap p = do
  vars <- traverse (uncurry createFree) (toList $ gatherFree p)
  pure $ fromList vars


makePredicate :: Pred 'TyPrimBool -> Symbolic (SBV Bool)
makePredicate p = do
  ss <- makeMap p
  pure $ SBV $ toSmt ss p


satisfiable :: Pred 'TyPrimBool -> Bool
satisfiable p = unsafePerformIO $ isSatisfiable $ makePredicate p


svPair :: forall p q. Editable p => Editable q => Proxy ('TyPrimPair p q) -> SVal -> SVal -> SVal
svPair _ x y = unSBV $ Smt.tuple ( SBV x :: SBV (TypeOf p), SBV y :: SBV (TypeOf q) )

svNil :: forall p. Editable p => Proxy ('TyPrimList p) -> SVal
svNil _ = unSBV (Smt.nil :: SList (TypeOf p))

svCons :: forall p. Editable p => Proxy ('TyPrimList p) -> SVal -> SVal -> SVal
svCons _ x xs = unSBV $ (Smt..:) (SBV x :: SBV (TypeOf p)) (SBV xs :: SList (TypeOf p))
