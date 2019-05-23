module Data.Something where


-- | Dynamic data abstracting over a constraint of kind `Type -> Constraint`
data Something c where
  Some :: ( Typeable a, c a ) => a -> Something c


instance Pretty (Something Pretty) where
  pretty (Some x) = sep [ "Some", pretty x ]


instance Eq (Something Eq) where
  Some x == Some y
    | Just Refl <- typeOf x ~~ typeOf y = x == y
    | otherwise = False


pack :: forall a c. ( Typeable a, c a ) => a -> Something c
pack = Some


unpack :: forall a c. Typeable a => Something c -> Maybe a
unpack (Some x)
  | Just Refl <- typeOf x ~~ r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep a


unsafeUnpack :: forall a c. Typeable a => Something c -> a
unsafeUnpack (Some x)
  | Just Refl <- typeOf x ~~ r = x
  | otherwise = error $ "Data.Some.unsafeUnpack: Types '" <> show (typeOf x) <> "' and '" <> show r <> "' did not match"
  where
    r = typeRep :: TypeRep a
