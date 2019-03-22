module Data.Task.Run where


import Control.Monad.Trace
import Control.Monad.Zero

import Data.Task
import Data.Task.Input as Input



-- Aliases ---------------------------------------------------------------------


type TaskIO = TaskT IO


-- type TaskST s = TaskT (ST s)



-- Observations ----------------------------------------------------------------


ui :: MonadRef m => TaskT m a -> m (Doc b)
ui = \case
  Edit (Just x) -> pure $ "□(" <> pretty x <> ")"
  Edit Nothing -> pure "□(_)"
  Store l -> do
    x <- readRef l
    pure $ "■(" <> pretty x <> ")"
  And left rght -> do
    l <- ui left
    r <- ui rght
    pure $ l <> "   ⋈   " <> r
  Or left rght -> do
    l <- ui left
    r <- ui rght
    pure $ l <> "   ◆   " <> r
  Xor left rght -> do
    l <- ui left
    r <- ui rght
    pure $ l <> " ◇ " <> r
  Fail -> pure "↯"
  Then this _ -> do
    t <- ui this
    pure $ t <> " ▶…"
  Next this _ -> do
    t <- ui this
    pure $ t <> " ▷…"


value :: MonadRef m => TaskT m a -> m (Maybe a)
value = \case
  Edit val      -> pure val
  Store loc     -> Just <$> readRef loc
  And left rght -> (<&>) <$> value left <*> value rght
  Or left rght  -> (<|>) <$> value left <*> value rght
  Xor _ _       -> pure Nothing
  Fail          -> pure Nothing
  Then _ _      -> pure Nothing
  Next _ _      -> pure Nothing


failing :: TaskT m a -> Bool
failing = \case
  Edit _        -> False
  Store _       -> False
  And left rght -> failing left && failing rght
  Or  left rght -> failing left && failing rght
  --TODO: fix to peek in future
  Xor left rght -> failing left && failing rght
  Fail          -> True
  Then this _   -> failing this
  Next this _   -> failing this


inputs :: forall m a. MonadZero m => MonadRef m => TaskT m a -> m (List (Input Dummy))
inputs = \case
  Edit _ ->
    pure [ ToHere (AChange tau), ToHere AEmpty ]
    where
      tau = Proxy :: Proxy a
  Store _ ->
    pure [ ToHere (AChange tau) ]
    where
      tau = Proxy :: Proxy a
  And left rght -> do
    l <- inputs left
    r <- inputs rght
    pure $ map ToFirst l ++ map ToSecond r
  Or left rght -> do
    l <- inputs left
    r <- inputs rght
    pure $ map ToFirst l ++ map ToSecond r
  Xor left rght ->
    pure $ map (ToHere << APick) choices
    where
      choices =
        case ( left, rght ) of
          ( Fail, Fail ) -> []
          ( _,    Fail ) -> [ GoLeft ]
          ( Fail, _ ) -> [ GoRight ]
          ( _,    _ ) -> [ GoLeft, GoRight ]
  Fail ->
    pure []
  Then this _ ->
    inputs this
  Next this next ->
    (++) <$> inputs this <*> [ [ToHere AContinue] | Just v <- value this, cont <- normalise (next v), not $ failing cont ]



-- Normalisation ---------------------------------------------------------------


stride ::
  MonadRef m =>
  TaskT m a -> m (TaskT m a)
stride = \case
  -- Step:
  Then this cont -> do
    this_new <- stride this
    val <- value this_new
    case val of
      Nothing -> pure $ Then this_new cont
      Just v  ->
        --FIXME: should we use stride here instead of just eval?
        let next = cont v in
        if failing next then
          pure $ Then this_new cont
        else
          stride next
  -- Evaluate:
  And left rght -> do
    left_new <- stride left
    rght_new <- stride rght
    pure $ And left_new rght_new
  Or left rght -> do
    left_new <- stride left
    val_left <- value left_new
    case val_left of
      Just _  -> pure left_new
      Nothing -> do
        rght_new <- stride rght
        val_rght <- value rght_new
        case val_rght of
          Just _  -> pure rght_new
          Nothing -> pure $ Or left_new rght_new
  Next this cont -> do
    this_new <- stride this
    pure $ Next this_new cont
  -- Values:
  task -> pure task


normalise :: MonadRef m => TaskT m a -> m (TaskT m a)
normalise = stride


initialise :: MonadRef m => TaskT m a -> m (TaskT m a)
initialise = normalise



-- Handling --------------------------------------------------------------------


data NotApplicable
  = CouldNotChange
  -- | CouldNotFind Label
  -- | CouldNotContinue
  | CouldNotHandle


handle :: forall m a.
  MonadTrace NotApplicable m => MonadRef m =>
  TaskT m a -> Input Action -> m (TaskT m a)

-- Edit --
handle (Edit _) (ToHere Empty) =
  pure $ Edit Nothing

handle (Edit val) (ToHere (Change val_new))
  -- NOTE: Here we check if `val` and `val_new` have the same type.
  -- If this is the case, it would be inhabited by `Refl :: a :~: b`, where `b` is the type of the value inside `Change`.
  -- Because we can't acces the type variable `b` directly, we use `~=` as a trick.
  | Just Refl <- val ~= val_new = pure $ Edit val_new
  | otherwise = trace CouldNotChange $ Edit val

handle (Store loc) (ToHere (Change val_ext))
  -- NOTE: As in the `Edit` case above, we check for type equality.
  -- Here, we can't annotate `Refl`, because we do not have acces to the type variable `b` inside `Store`.
  -- We also do not have acces to the value stored in `loc` (we could readRef it first using `readRef`).
  -- Therefore we use a proxy `Nothing` of the correct scoped type to mach against the type of `val_ext`.
  | Just Refl <- (Nothing :: Maybe a) ~= val_ext =
      case val_ext of
        Just val_new -> do
          loc $= const val_new
          pure $ Store loc
        Nothing ->
          trace CouldNotChange $ Store loc
  | otherwise =
      trace CouldNotChange $ Store loc

-- Pass to left or rght --
handle (And left rght) (ToFirst input) = do
  -- Pass the input to left
  left_new <- handle left input
  pure $ And left_new rght

handle (And left rght) (ToSecond input) = do
  -- Pass the input to rght
  rght_new <- handle rght input
  pure $ And left rght_new

handle (Or left rght) (ToFirst input) = do
  -- Pass the input to left
  left_new <- handle left input
  pure $ Or left_new rght

handle (Or left rght) (ToSecond input) = do
  -- Pass the input to rght
  rght_new <- handle rght input
  pure $ Or left rght_new

-- Interact --
handle (Xor left _) (ToHere (Pick GoLeft)) =
  -- Go left
  --FIXME: add failing for left?
  pure left

handle (Xor _ rght) (ToHere (Pick GoRight)) =
  -- Go rght
  --FIXME: add failing for rght?
  pure rght

-- handle task@(Xor _ _) (ToHere (PickWith l)) =
--   case find l task of
--     Nothing -> trace (CouldNotFind l) task
--     --XXX: needs `assert_smaller` for totallity, be aware of long type checks...
--     -- Just p  -> handle task (assert_smaller (ToHere (PickWith l)) (ToHere (Pick p)))
--     Just p  -> handle task (ToHere (Pick p))

handle (Next this cont) (ToHere Continue) =
  -- When pressed continue rewrite to an internal step
  pure $ Then this cont

-- handle task@(Next this cont) (ToHere (ContinueWith l)) =
--   case !(value this) of
--     Nothing -> trace CouldNotContinue task
--     Just v  ->
--       let
--         next = cont v
--       in
--       case find l next of
--         Nothing -> trace (CouldNotFind l) task
--         Just p  -> handle next (ToHere (Pick p))

-- Pass to this --
handle (Then this cont) input = do
  -- Pass the input to this
  this_new <- handle this input
  pure $ Then this_new cont

handle (Next this cont) input = do
  -- Pass the input to this
  this_new <- handle this input
  pure $ Next this_new cont

-- Rest --
handle task _ =
  -- trace (CouldNotHandle input) task
  trace CouldNotHandle task


drive :: MonadTrace NotApplicable m => MonadRef m => TaskT m a -> Input Action -> m (TaskT m a)
drive task input =
  handle task input >>= normalise



-- Running ---------------------------------------------------------------------


getInput :: IO (Input Action)
getInput = do
  putText ">> "
  input <- getLine
  case input of
    "quit" -> exitSuccess
    _ ->
      case Input.parse (words input) of
        Right i -> pure i
        Left msg -> do
          print msg
          getInput


{-
loop :: TaskIO a -> IO ()
loop task = do
  ui <- Task.ui task
  putStrLn ui
  is <- Task.inputs task
  putStrLn $ "Possibilities: " <> show is
  input <- getInput
  task_new <- _ task input
  loop task_new


run :: Show (typeOf a) -> Task a -> IO ()
run task = loop !(Task.initialise task)


main :: IO ()
main = run empties

-}
