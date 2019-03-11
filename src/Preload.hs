{-# LANGUAGE UndecidableInstances #-}
module Preload
  ( module Protolude
  , module Data.Bitraversable
  , List, Unit
  , quote, paren, words, unwords, lines, unlines
  , Debug(..), read
  , neutral
  , (<<), (>>), (#), (<#>)
  , (<-<), (>->), (<&>), skip
  , forany, forall, ok, throw, catch
  , sameT, proxyOf, typeOf
  ) where


import Protolude hiding
  ( (.), (>>), (&), (<&>)
  , catch, handle
  )

import Data.Bitraversable
import Data.Text (pack, unpack, words, unwords, lines, unlines)

import Type.Reflection (typeOf)



-- Synonyms --------------------------------------------------------------------


type List a = [a]


type Unit = ()



-- Text ------------------------------------------------------------------------


quote :: Text -> Text
quote t = "`" <> t <> "`"


paren :: Text -> Text
paren t = "(" <> t <> ")"



-- Reading & Showing -----------------------------------------------------------


class Debug a where
  debug :: a -> Text


instance {-# OVERLAPPABLE #-} (Show a) => Debug a where
  debug = pack << show


read :: Read a => Text -> Maybe a
read = readMaybe << unpack



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


-- | A version of 'any' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > forany [False,True ,undefined] Just == Just True
-- > forany [False,False,undefined] Just == undefined
-- > xs \(f :: Int -> Maybe Bool) -> forany xs f == orM (map f xs)
forany :: Monad m => List a -> (a -> m Bool) -> m Bool
forany [] _ = return False
forany (x:xs) p = ifM (p x) (return True) (forany xs p)


-- | A version of 'all' lifted to a monad. Retains the short-circuiting behaviour.
--
-- > forall [True,False,undefined] Just == Just False
-- > forall [True,True ,undefined] Just == undefined
-- > xs \(f :: Int -> Maybe Bool) -> forany xs f == orM (map f xs)
forall :: Monad m => List a -> (a -> m Bool) -> m Bool
forall [] _ = return True
forall (x:xs) p = ifM (p x) (forall xs p) (return False)



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
