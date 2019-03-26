module Data.Task
  ( TaskT(..), Task
  , edit, enter, view, update, watch
  , tmap, (-&&-), (&&-), (-&&), (-||-), (-??-), fail, (>>-), (>>?)
  , module Control.Monad.Ref
  , module Data.Basic
  ) where


import Control.Monad.Ref

import Data.Basic (Basic)


infixl 5 -&&-, -&&, &&-
infixl 3 -||-, -??-
infixl 2 >>-, >>?



-- Tasks -----------------------------------------------------------------------


data TaskT (m :: Type -> Type) (r :: Type) where
  -- | Editors, valued or unvalued
  Edit :: Basic r => Maybe r -> TaskT m r

  -- | Stores referring to some shared value of type `r`
  Store :: ( MonadRef m, Eq (Ref m r), Basic r ) => Ref m r -> TaskT m r

  -- | Composition of two tasks.
  And :: TaskT m a -> TaskT m b -> TaskT m ( a, b )

  -- | Internal choice between two tasks.
  Or :: TaskT m r -> TaskT m r -> TaskT m r

  -- | External choice between two tasks.
  Xor :: TaskT m r -> TaskT m r -> TaskT m r

  -- | The failing task
  Fail :: TaskT m r

  -- | Internal, or system step.
  Then :: TaskT m a -> (a -> TaskT m r) -> TaskT m r

  -- | External, or user step.
  Next :: TaskT m a -> (a -> TaskT m r) -> TaskT m r

  --FIXME: add labels
  --FIXME: add tmap and program some examples


type Task = TaskT IO



-- Instances -------------------------------------------------------------------

-- Show --

instance Pretty (TaskT m t) where
  pretty = \case
    Edit (Just x) -> cat [ "□(", pretty x, ")" ]
    Edit Nothing -> "□(_)"
    Store _ -> "■(_)"
    And x y -> sep [ pretty x, "⋈", pretty y ]
    Or x y -> sep [ pretty x, "◆", pretty y ]
    Xor x y -> sep [ pretty x, "◇", pretty y ]
    Fail -> "↯"
    Then x _ -> sep [ pretty x, "▶…" ]
    Next x _ -> sep [ pretty x, "▷…" ]



-- Functor --


tmap :: Basic b => (a -> b) -> TaskT m a -> TaskT m b
tmap f t = (>>-) t (edit << f)



-- Applicative --


edit :: Basic a => a -> TaskT m a
edit x = Edit (Just x)


enter :: Basic a => TaskT m a
enter = Edit Nothing


view :: Basic a => a -> TaskT m a
view = edit


update :: MonadRef m => Eq (Ref m a) => Basic a => Ref m a -> TaskT m a
update = Store


watch :: MonadRef m => Eq (Ref m a) => Basic a => Ref m a -> TaskT m a
watch = update


(-&&-) :: TaskT m a -> TaskT m b -> TaskT m ( a, b )
(-&&-) = And


(-&&) :: Basic a => TaskT m a -> TaskT m b -> TaskT m a
x -&& y = tmap fst $ x -&&- y


(&&-) :: Basic b => TaskT m a -> TaskT m b -> TaskT m b
x &&- y = tmap snd $ x -&&- y


-- apply' :: TaskT m (a -> b) -> TaskT m a -> TaskT m b
-- apply' ff fx = tmap (\(Tuple f x) -> f x) $ ff <&> fx



-- Alternative --


fail :: TaskT m a
fail = Fail


(-||-) :: TaskT m a -> TaskT m a -> TaskT m a
(-||-) = Or


(-??-) :: TaskT m a -> TaskT m a -> TaskT m a
(-??-) = Xor



-- Monad --


(>>-) :: TaskT m b -> (b -> TaskT m a) -> TaskT m a
(>>-) = Then


(>>?) :: TaskT m a -> (a -> TaskT m b) -> TaskT m b
(>>?) = Next
