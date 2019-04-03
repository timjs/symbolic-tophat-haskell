module Control.Monad.Ref
  ( MonadRef(..), ($=)
  , Someref, pack, unpack
  ) where



-- Interface -------------------------------------------------------------------


class Monad m => MonadRef m where
  type Ref m :: Type -> Type

  ref    :: a -> m (Ref m a)
  deref  :: Ref m a -> m a
  assign :: Ref m a -> a -> m ()


infix 4 $=
($=) :: MonadRef m => Ref m a -> (a -> a) -> m ()
l $= f = do
  x <- deref l
  assign l (f x)


instance MonadRef IO where
  type Ref IO = IORef

  ref    = newIORef
  deref  = readIORef
  assign = writeIORef



-- Existential packing ---------------------------------------------------------


data Someref (m :: Type -> Type) where
  Someref :: ( Eq (Ref m a), Typeable a ) => TypeRep a -> Ref m a -> Someref m


pack :: forall m a. ( Eq (Ref m a), Typeable a ) => Ref m a -> Someref m
pack = Someref typeRep


unpack :: forall m a. Typeable a => Someref m -> Maybe (Ref m a)
unpack (Someref r x)
  | Just Refl <- r ~~ t' = Just x
  | otherwise = Nothing
  where
    t' = typeRep :: TypeRep a


instance Eq (Someref m) where
  Someref rx x == Someref ry y
    | Just Refl <- rx ~~ ry = x == y
    | otherwise = False
