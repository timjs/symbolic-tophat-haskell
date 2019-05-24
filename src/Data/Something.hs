module Data.Something where


-- | Dynamic data abstracting over a constraint of kind `Type -> Constraint`
data Something c where
  Some :: ( c a ) => a -> Something c


pack :: forall a c. ( c a ) => a -> Something c
pack = Some
