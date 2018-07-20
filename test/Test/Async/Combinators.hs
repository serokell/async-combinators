module Test.Async.Combinators where

import Control.Concurrent (killThread, myThreadId, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (AsyncException (ThreadKilled))
import Control.Exception.Safe (Exception, catch, catchAny, handleAsync, isAsyncException, throw,
                               throwTo, tryAsync)
import Control.Monad (forever)
import Data.Either (isRight)
import Data.IORef (newIORef, readIORef, writeIORef)

import Test.HUnit (Assertion, assertEqual, assertFailure, (@?))

import Async.Combinators


data TestException = TestException
    deriving Show

instance Exception TestException

-----------------------
-- withWorker
-----------------------

data WorkerTestResult = WorkerTestResult
    { wtrReturn     :: Either WorkerExited ()
    , wtrWorkerDone :: Bool
    , wtrGoDone     :: Bool
    }

withWorker_test :: IO () -> IO () -> IO WorkerTestResult
withWorker_test worker go = do
    workerDone <- newIORef False
    goDone     <- newIORef False

    r <- tryAsync $ withWorker "worker" (worker *> writeIORef workerDone True)
                                        (go *> writeIORef goDone True)

    WorkerTestResult r <$> readIORef workerDone <*> readIORef goDone

assertWtr :: Bool -> Bool -> Bool -> WorkerTestResult -> Assertion
assertWtr expectRight expectWorkerDone expectGoDone WorkerTestResult{..} = do
    assertEqual "return right" expectRight (isRight wtrReturn)
    assertEqual "worker done"  expectWorkerDone wtrWorkerDone
    assertEqual "go done"      expectGoDone wtrGoDone


--------
-- First, some sanity checks.
--------

unit_withWorker_exception_is_async :: Assertion
unit_withWorker_exception_is_async = do
    isAsyncException (WorkerExited "hi") @? "isAsync"

    me <- myThreadId
    r <- tryAsync $ catchAny (throwTo me $ WorkerExited "hi") (const $ pure ())
    case r of
        Right _                  -> assertFailure $ "caught by safe `catchAny`"
        Left (_ :: WorkerExited) -> pure ()


unit_withWorker_exception_can_be_caught :: Assertion
unit_withWorker_exception_can_be_caught = do
    me <- myThreadId
    handleAsync (\(_ :: WorkerExited) -> pure ()) $ throwTo me (WorkerExited "bye")

--------
-- Now actual tests.
--------

-- | Main thread exits -> worker is killed.
unit_withWorker_main_exit :: Assertion
unit_withWorker_main_exit =
    withWorker_test (forever $ threadDelay 100) (threadDelay 100) >>=
    assertWtr True False True

-- | Main crashes -> worker is killed.
unit_withWorker_main_crash :: Assertion
unit_withWorker_main_crash =
    withWorker_test (forever $ threadDelay 100) (throw TestException `catch` \TestException -> putStr "Hi") >>=
    assertWtr True False True

-- | Worker exits -> main thread is killed.
unit_withWorker_worker_exit :: Assertion
unit_withWorker_worker_exit =
    withWorker_test (pure ()) (threadDelay 100) >>=
    assertWtr False True False

-- | Worker crashes -> main thread is killed.
unit_withWorker_worker_crash :: Assertion
unit_withWorker_worker_crash =
    withWorker_test (throw TestException) (threadDelay 100000) >>=
    assertWtr False False False

-- | Worker receives an async exception -> main thread is killed.
unit_withWorker_worker_crash_async :: Assertion
unit_withWorker_worker_crash_async =
    withWorker_test worker (threadDelay 100000) >>=
    assertWtr False False False
  where
    worker = myThreadId >>= \me -> throwTo me TestException

-- | Worker receives ThreadKilled -> main thread is killed.
unit_withWorker_worker_crash_ThreadKilled :: Assertion
unit_withWorker_worker_crash_ThreadKilled =
    withWorker_test worker (threadDelay 100000) >>=
    assertWtr False False False
  where
    worker = myThreadId >>= \me -> throwTo me ThreadKilled

-- | Main thread kills worker -> main thread is killed.
unit_withWorker_main_kills_worker :: Assertion
unit_withWorker_main_kills_worker = do
    v <- newEmptyMVar
    wtr <- withWorker_test (myThreadId >>= putMVar v  >> threadDelay 1000000)
                           (takeMVar v >>= killThread >> threadDelay 1000000)
    assertWtr False False False wtr
