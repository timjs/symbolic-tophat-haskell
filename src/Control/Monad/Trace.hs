module Control.Monad.Trace where

import Control.Monad.List
import Control.Monad.Supply
import Control.Monad.Writer


-- Class -----------------------------------------------------------------------

class ( Pretty a, Monad m ) => MonadTrace a m where
  trace :: a -> m ()


-- Instances -------------------------------------------------------------------

instance Pretty a => MonadTrace a IO where
  trace x = do
    putStrLn $ "** " <> show (pretty x)

instance MonadTrace a m => MonadTrace a (ListT m) where
  trace = lift << trace

instance MonadTrace a m => MonadTrace a (StateT s m) where
  trace = lift << trace

instance MonadTrace a m => MonadTrace a (SupplyT s m) where
  trace = lift << trace

instance MonadTrace a m => MonadTrace a (ReaderT s m) where
  trace = lift << trace

instance ( Monad m, Pretty a ) => MonadTrace a (WriterT (List (Doc n)) m) where
  trace x = do
    tell [ pretty x ]

type TracerT = WriterT (List (Doc ()))
type Tracer = TracerT Identity

runTracerT :: TracerT m a -> m ( a, List (Doc ()) )
runTracerT = runWriterT

runTracer :: Tracer a -> ( a, List (Doc ()) )
runTracer = runWriter
