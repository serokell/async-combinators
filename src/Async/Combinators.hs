module Async.Combinators
       ( -- * Running forever
         WorkerExited (WorkerExited, WorkerFailed)
       , withWorker
       ) where

import Universum

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (withAsync)
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Exception.Safe (Exception (..), finally, throwTo, tryAsync)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)

import qualified GHC.Show as Show (Show (show))


-----------------------
-- Running forever
-----------------------

-- | Asynchronous exception thrown to the main thread if the worker exits.
data WorkerExited = WorkerExited Text                -- ^ Worker returned
                  | WorkerFailed Text SomeException  -- ^ Worker crashed

instance Show WorkerExited where
    show (WorkerExited n)   = toString $ "Worker '" <> n <> "' returned"
    show (WorkerFailed n e) = toString $ "Worker '" <> n <> "' failed: " <> show e

instance Exception WorkerExited where
    toException   = asyncExceptionToException
    fromException = asyncExceptionFromException

-- | Like 'withAsync', but makes sure that the worker thread will
-- not exit, neither by returning, nor by throwing an exception.
-- If it exits, a 'WorkerExited' exception will be thrown in the current thread.
withWorker :: MonadUnliftIO m
           => Text  -- ^ Human-readable name for the forked thread
           -> m ()  -- ^ Action performed by the worker
           -> m b   -- ^ Action performed in current thread
           -> m b
withWorker name worker go = withRunInIO $ \run -> do
    tid <- myThreadId
    mainDone <- newIORef False
    let worker' = do
            res <- tryAsync $ run worker
            unlessM (readIORef mainDone) $ throwTo tid $
                case res of
                    Right () -> WorkerExited name
                    Left  e  -> WorkerFailed name e
    withAsync worker' $ \_ -> run go `finally` atomicWriteIORef mainDone True
