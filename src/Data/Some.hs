module Data.Some
  ( Some(..), Any
  , pack, unpack, unsafeUnpack
  ) where


data Some c f where
  Some :: ( Typeable (f a), c a ) => f a -> Some c f

type Any f = Some Typeable f  -- FIXME: Better: empty constraint, but how?


instance ( forall a. Pretty (f a) ) => Pretty (Some c f) where
  pretty (Some x) = sep [ "Some", pretty x ]

instance ( forall a. Eq (f a) ) => Eq (Some c f) where
  Some x == Some y
    | Just Refl <- typeOf x ~~ typeOf y = x == y
    | otherwise = False

instance ( forall a. Hashable (f a) ) => Hashable (Some c f) where
  hashWithSalt n (Some x) = hashWithSalt n x


pack :: forall f a c. Typeable (f a) => c a => f a -> Some c f
pack = Some

unpack :: forall f a c. Typeable (f a) => Some c f -> Maybe (f a)
unpack (Some x)
  | Just Refl <- typeOf x ~~ r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (f a)

unsafeUnpack :: forall f a c. Typeable (f a) => Some c f -> f a
unsafeUnpack (Some x)
  | Just Refl <- typeOf x ~~ r = x
  | otherwise = error $
      "Data.Some.unsafeUnpack: Types '" <> show (typeOf x) <>
      "' and '" <> show r <> "' did not match"
  where
    r = typeRep :: TypeRep (f a)
