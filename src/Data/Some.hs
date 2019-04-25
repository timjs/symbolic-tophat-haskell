module Data.Some where


data Some f where
  Some :: Typeable (f a) => f a -> Some f


pack :: forall f a. Typeable (f a) => f a -> Some f
pack = Some


unpack :: forall f a. Typeable (f a) => Some f -> Maybe (f a)
unpack (Some x)
  | Just Refl <- typeOf x ~~ r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (f a)


unsafeUnpack :: forall f a. Typeable (f a) => Some f -> f a
unsafeUnpack (Some x)
  | Just Refl <- typeOf x ~~ r = x
  | otherwise = error $ "Data.Some.unsafeUnpack: Types '" <> show (typeOf x) <> "' and '" <> show r <> "' did not match"
  where
    r = typeRep :: TypeRep (f a)
