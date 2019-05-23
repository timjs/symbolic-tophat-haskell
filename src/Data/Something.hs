module Data.Something where


-- | Dynamic data abstracting over a constraint of kind `Type -> Constraint`
data Something c where
  Some :: ( c a ) => a -> Something c


-- instance Pretty (Something Pretty) where
--   pretty (Some x) = sep [ "Some", pretty x ]


-- instance Eq (Something Eq) where
--   Some x == Some y
--     | Just Refl <- typeOf x ~~ typeOf y = x == y
--     | otherwise = False


pack :: forall a c. ( c a ) => a -> Something c
pack = Some
