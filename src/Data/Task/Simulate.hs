module Data.Task.Simulate where

{-
import Text.Read (Read(..))

import Control.Monad.Ref
import Data.SBV.Trans
import Data.Task
import Data.Task.Input



-- Simulating ------------------------------------------------------------------


simulate :: forall m a.
  MonadIO m => MonadRef m =>
  -- TaskT m a -> SymbolicT m (TaskT m (SBV a))
  -- TaskT m a -> SymbolicT (TaskT m) (SBV a)
  TaskT m a -> TaskT (SymbolicT m) (SBV a)
simulate = \case
  Edit _         -> _
  Store loc      -> _
  And left rght  -> _
  Or  left rght  -> _
  Xor left rght  -> _ --Lift free_ `Then` \b -> simulate left --ite b (simulate left) (simulate rght)
  Fail           -> Fail
  Then this cont -> _
  Next this cont -> _



-- Boilerplate -----------------------------------------------------------------


instance Pretty (SBV a) where
  pretty = viaShow


instance Read a => Read (SBV a) where
  readPrec = _

-}
