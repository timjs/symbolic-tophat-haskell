module Prelude
  ( module Relude
  , module Data.Text.Prettyprint.Doc
  , module Data.Type.Equality
  , List, Unit
  , Pretty(..), read
  , neutral
  , (<<), (>>), (#), (<#>), map
  , (<-<), (>->), (<&>), skip
  , lift1, lift2, lift3
  , ok, throw, catch
  , (~=), proxyOf, typeOf, typeRep, TypeRep
  ) where


import Relude hiding ((.), (>>), (&), (<&>), map, fail, readMaybe, liftA2, liftA3)
import qualified Relude

import Control.Monad.Except (MonadError(..))

import Data.Text (unpack)
import Data.Text.Prettyprint.Doc hiding (group)
import Data.Type.Equality

import Type.Reflection (typeOf, typeRep, TypeRep)



-- Synonyms --------------------------------------------------------------------


type List a = [a]


type Unit = ()



-- Reading & Showing -----------------------------------------------------------


read :: Read a => Text -> Maybe a
read = Relude.readMaybe << unpack



-- Monoids ---------------------------------------------------------------------


{-# INLINE neutral #-}
neutral :: Monoid m => m
neutral = mempty



-- Functions -------------------------------------------------------------------


infixr 9 <<
{-# INLINE (<<) #-}
(<<) :: (b -> c) -> (a -> b) -> a -> c
f << g = \x -> f (g x)


infixr 9 >>
{-# INLINE (>>) #-}
(>>) :: (a -> b) -> (b -> c) -> a -> c
(>>) = flip (<<)


infixl 1 #
{-# INLINE (#) #-}
(#) :: a -> (a -> b) -> b
(#) = flip ($)



-- Functors --------------------------------------------------------------------


infixl 1 <#>
{-# INLINE (<#>) #-}
(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip (<$>)


map :: Functor f => (a -> b) -> f a -> f b
map = Relude.fmap



-- Applicatives ----------------------------------------------------------------


lift1 :: Functor f => (a -> b) -> f a -> f b
lift1 = fmap


lift2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
lift2 = Relude.liftA2


lift3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3 = Relude.liftA3


infixr 1 <-<
{-# INLINE (<-<) #-}
(<-<) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
f <-< g = (<<) <$> f <*> g


infixr 1 >->
{-# INLINE (>->) #-}
(>->) :: Applicative f => f (a -> b) -> f (b -> c) -> f (a -> c)
(>->) = flip (<-<)


infixl 5 <&>
{-# INLINE (<&>) #-}
(<&>) :: Applicative f => f a -> f b -> f ( a, b )
(<&>) x y = (,) <$> x <*> y


{-# INLINE skip #-}
skip :: Applicative f => f ()
skip = pure ()



-- Monads ----------------------------------------------------------------------

-- Errors --


{-# INLINE ok #-}
ok :: MonadError e m => a -> m a
ok = pure


{-# INLINE throw #-}
throw :: MonadError e m => e -> m a
throw = throwError


{-# INLINE catch #-}
catch :: MonadError e m => m a -> (e -> m a) -> m a
catch = catchError



-- Type equality ---------------------------------------------------------------


-- {-# INLINE sameT #-}
-- sameT :: ( Typeable a, Typeable b ) => a -> b -> Maybe (a :~: b)
-- sameT _ _ = eqT


{-# INLINE proxyOf #-}
proxyOf :: a -> Proxy a
proxyOf _ = Proxy


infix 4 ~=
(~=) :: TestEquality f => f a -> f b -> Maybe (a :~: b)
(~=) = testEquality
