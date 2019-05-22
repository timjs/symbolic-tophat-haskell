module Prelude
  ( module Relude
  , module Data.Text.Prettyprint.Doc
  , module Data.Type.Equality
  , List, Unit, Nat
  , scan, tracePretty, split
  , neutral
  , (<<), (>>), (#), map
  , (<-<), (>->)
  , Monoidal((<&>), skip, (<&), (&>)), applyDefault, pureDefault
  , Selective(branch, select, biselect), check, when
  , lift1, lift2, lift3
  , MonadZero
  , ok, throw, catch
  , clear, evalWriterT
  , MonadTrace, trace
  , (~=), (~~), proxyOf, typeOf, someTypeOf, typeRep, TypeRep, someTypeRep, SomeTypeRep
  ) where


import Relude hiding ((.), (>>), (&), (<&>), (<$>), map, when, pass, trace, readMaybe, liftA2, liftA3, Nat)

import Control.Monad.Except (MonadError(..))
import Control.Monad.List (ListT)
import Control.Monad.Writer.Strict (MonadWriter(..), WriterT, runWriterT)

import Data.Text (unpack)
import Data.Text.Prettyprint.Doc (Pretty(..), Doc, cat, sep, indent, parens, angles)
import Data.Type.Equality

import Type.Reflection (typeOf, typeRep, someTypeRep, TypeRep, SomeTypeRep)

import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Relude


-- Synonyms --------------------------------------------------------------------


type List = []


type Unit = ()


newtype Nat = Nat Word
  deriving ( Eq, Ord, Num, Show, Read, Enum, Pretty ) via Word



-- Scanning & Printing ---------------------------------------------------------


scan :: Read a => Text -> Maybe a
scan = Relude.readMaybe << unpack


tracePretty :: Pretty a => a -> a
tracePretty x = traceShow (pretty x) x


split :: List (Doc ann) -> Doc ann
split = Pretty.vsep



-- Monoids ---------------------------------------------------------------------


neutral :: Monoid m => m
neutral = mempty
{-# INLINE neutral #-}



-- Functions -------------------------------------------------------------------


infixr 9 <<
infixr 9 >>
infixl 1 #


(<<) :: (b -> c) -> (a -> b) -> a -> c
f << g = \x -> f (g x)
{-# INLINE (<<) #-}


(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (<<)
{-# INLINE (>>) #-}


(#) :: a -> (a -> b) -> b
(#) = flip ($)
{-# INLINE (#) #-}



-- Functors --------------------------------------------------------------------


-- infixl 1 <#>


-- (<#>) :: Functor f => f a -> (a -> b) -> f b
-- (<#>) = flip map
-- {-# INLINE (<#>) #-}


map :: Functor f => (a -> b) -> f a -> f b
map = Relude.fmap



-- Applicatives ----------------------------------------------------------------


infixr 1 <-<
infixr 1 >->


lift0 :: Applicative f => a -> f a
lift0 = pure
{-# INLINE lift0 #-}


lift1 :: Applicative f => (a -> b) -> f a -> f b
lift1 = fmap
{-# INLINE lift1 #-}


lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 = Relude.liftA2
{-# INLINE lift2 #-}


lift3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 = Relude.liftA3
{-# INLINE lift3 #-}


(<-<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
f <-< g = pure (<<) <*> f <*> g
{-# INLINE (<-<) #-}


(>->) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>->) = flip (<-<)
{-# INLINE (>->) #-}



-- Monoidal functors -----------------------------------------------------------


infixl 6 <&>
infixl 6 <&
infixl 6 &>


class Applicative f => Monoidal f where
  (<&>) :: f a -> f b -> f ( a, b )
  (<&>) x y = pure (,) <*> x <*> y

  skip :: f ()
  skip = pure ()

  (<&) :: f a -> f b -> f a
  (<&) x y = map fst $ x <&> y

  (&>) :: f a -> f b -> f b
  (&>) x y = map snd $ x <&> y


applyDefault :: Monoidal f => f (a -> b) -> f a -> f b
applyDefault fg fx = map (\( g, x ) -> g x) $ fg <&> fx


pureDefault :: Monoidal f => a -> f a
pureDefault x = map (const x) skip


instance Monoidal Maybe
instance Monoidal (Either e)
instance Monoidal IO



-- Selective functors ----------------------------------------------------------


class Applicative f => Selective f where
  branch :: f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
  branch p x y = map (map Left) p `select` map (map Right) x `select` y

  select :: f (Either a b) -> f (a -> b) -> f b
  select x y = branch x y (pure identity)

  biselect :: f (Either a b) -> f (Either a c) -> f (Either a ( b, c ))
  biselect x y = select (pure (map Left << swp) <*> x) (pure (\e a -> map ( a, ) e) <*> y)
    where
      swp = either Right Left


check :: Selective f => f Bool -> f a -> f a -> f a
check p t e = branch (map go p) (map const t) (map const e)
  where
    go x = if x then Right () else Left ()


when :: Selective f => f Bool -> f Unit -> f Unit
when p t = check p t (pure ())



-- Monads ----------------------------------------------------------------------

-- Zero --

-- | A safe alternative for MonadFail.
-- |
-- | Be sure only types that have a sensible, non-throwing,
-- | `Alternative` instance are instances of `MonadZero`!
-- |
-- | Laws:
-- |   * fail == empty
class ( Monad m, MonadFail m, Alternative m ) => MonadZero m

instance MonadZero Maybe
instance MonadZero List
instance Monad m => MonadZero (ListT m)



-- Error --


ok :: MonadError e m => a -> m a
ok = pure
{-# INLINE ok #-}


throw :: MonadError e m => e -> m a
throw = throwError
{-# INLINE throw #-}


catch :: MonadError e m => m a -> (e -> m a) -> m a
catch = catchError
{-# INLINE catch #-}


-- Writer --


clear :: MonadWriter w m => m ()
clear = pass $ lift0 ((), const neutral)


evalWriterT :: Monad m => WriterT w m a -> m a
evalWriterT m = lift1 fst (runWriterT m)


instance MonadWriter w m => MonadWriter w (ListT m) where
  tell = lift << tell
  listen = error "Prelude.listen for ListT m: not yet implemented"
  pass = error "Prelude.pass for ListT m: not yet implemented"


-- Trace --

type MonadTrace i m = (Pretty i, MonadWriter (List (Doc ())) m)

trace :: MonadTrace i m => i -> m ()
trace = tell << pure << pretty


-- Type equality ---------------------------------------------------------------


infix 4 ~=
infix 4 ~~


(~=) :: ( Typeable a, Typeable b ) => a -> b -> Maybe (a :~: b)
(~=) x y = typeOf x ~~ typeOf y
{-# INLINE (~=) #-}


(~~) :: TestEquality f => f a -> f b -> Maybe (a :~: b)
(~~) = testEquality
{-# INLINE (~~) #-}


proxyOf :: a -> Proxy a
proxyOf _ = Proxy
{-# INLINE proxyOf #-}


someTypeOf :: forall a. Typeable a => a -> SomeTypeRep
someTypeOf = someTypeRep << proxyOf
{-# INLINE someTypeOf #-}


instance Pretty SomeTypeRep where
  pretty = Pretty.viaShow
