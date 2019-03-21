module Data.Basic
  ( Basic
  , Somebasic, pack, unpack, unsafeUnpack
  ) where



-- Class -----------------------------------------------------------------------


type Basic a = ( Pretty a, Read a, Eq a, Typeable a ) -- , Arbitrary a ) -- Coarbitrary a )



-- Packing and unpacking -------------------------------------------------------


data Somebasic :: Type where
  Somebasic :: forall a. Basic a => TypeRep a -> a -> Somebasic


pack :: forall a. Basic a => a -> Somebasic
pack = Somebasic typeRep


unpack :: forall a. Basic a => Somebasic -> Maybe a
unpack (Somebasic r x)
  | Just Refl <- r ~= r' = Just x
  | otherwise = Nothing
  where
    r' = typeRep :: TypeRep a


unsafeUnpack :: forall a. Basic a => Somebasic -> a
unsafeUnpack (Somebasic r x)
  | Just Refl <- r ~= r' = x
  | otherwise = error $ "Data.Basic.unsafeUnpack: Types '" <> show r <> "' and '" <> show r' <> "' did not match"
  where
    r' = typeRep :: TypeRep a



-- Instances -------------------------------------------------------------------


instance Pretty Somebasic where
  pretty (Somebasic _ x) = pretty x


instance Eq Somebasic where
  (Somebasic r x) == (Somebasic s y)
    | Just Refl <- r ~= s = x == y
    | otherwise = False
