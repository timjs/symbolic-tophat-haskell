module Prelude
  ( module Relude
  , List, Unit
  , Pretty(..), read
  , neutral
  , (<<), (>>), (#), (<#>)
  , (<-<), (>->), (<&>), skip
  , ok, throw, catch
  , sameT, proxyOf, typeOf
  ) where


import Relude hiding ((.), (>>), (&), (<&>), readMaybe)
import qualified Relude

import Control.Monad.Except (MonadError(..))

import Data.Text (unpack)
import Data.Text.Prettyprint.Doc
import Data.Typeable (eqT)

import Type.Reflection



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



-- Applicatives ----------------------------------------------------------------


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


{-# INLINE sameT #-}
sameT :: ( Typeable a, Typeable b ) => a -> b -> Maybe (a :~: b)
sameT _ _ = eqT


{-# INLINE proxyOf #-}
proxyOf :: a -> Proxy a
proxyOf _ = Proxy
