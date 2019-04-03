module Data.Task.Simulate where


import Text.Read (Read(..))

import Control.Monad.Ref
import Data.SBV.Trans
import Data.Task
import Data.Task.Input



-- Simulating ------------------------------------------------------------------


-- simulate :: forall m a.
--   MonadIO m => MonadRef m =>
--   -- TaskT m a -> SymbolicT m (TaskT m (SBV a))
--   -- TaskT m a -> SymbolicT (TaskT m) (SBV a)
--   TaskT m a -> TaskT (SymbolicT m) (SBV a)
-- simulate = \case
--   Edit _         -> _
--   Store loc      -> _
--   And left rght  -> _
--   Or  left rght  -> _
--   Xor left rght  -> _ --Lift free_ `Then` \b -> simulate left --ite b (simulate left) (simulate rght)
--   Fail           -> Fail
--   Then this cont -> _
--   Next this cont -> _



-- Boilerplate -----------------------------------------------------------------


instance Pretty (SBV a) where
  pretty = viaShow


instance Read a => Read (SBV a) where
  readPrec = undefined


-- instance Mergeable (TaskT m a) where
--   symbolicMerge f p t1 t2 = case ( t1, t2 ) of
--     ( Edit v1   , Edit v2    ) -> Edit  (symbolicMerge f p v1 v2)
--     ( Store l1  , Store l2   ) -> Store (symbolicMerge f p l1 l2)
--     ( And l1 r1 , And l2 r2  ) -> And   (symbolicMerge f p l1 l2) (symbolicMerge f p l1 l2)
--     ( Or l1 r1  , Or l2 r2   ) -> Or    (symbolicMerge f p l1 l2) (symbolicMerge f p r1 r2)
--     ( Xor l1 r1 , Xor l2 r2  ) -> Xor   (symbolicMerge f p l1 l2) (symbolicMerge f p r1 r2)
--     ( Fail      , Fail       ) -> Fail
--     ( Then t1 c1, Then t2 c2 ) -> Then  (symbolicMerge f p t1 t2) (symbolicMerge f p r1 r2)
--     ( Next t1 c1, Next t2 c2 ) -> Next  (symbolicMerge f p t1 t2) (symbolicMerge f p r1 r2)
