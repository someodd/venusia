-- | This module provides a file watcher that intelligently handles file change
-- events.
--
-- The main challenge with file watching is that a single "save" action can
-- create a storm of low-level system events. This watcher solves that problem by
-- "debouncing" the storm—it reacts only to the first event and ignores all
-- others for a set period, ensuring a command runs only once per actual change.
module Venusia.Server.Watcher (watchForChanges, WatchHook (..)) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, swapMVar, tryTakeMVar)
import Control.Monad (forever, void, when)
import Data.Maybe (fromMaybe)
import System.FSNotify (watchTree, withManager)
import System.FilePath ((</>))
import System.Process (callCommand)
import Venusia.Gateway (loadGatewayRoutes)
import Venusia.Server (Route)

-- | Describes a command to run when a file change is detected.
--
-- Think of this as a recipe for what to do after a file save.
data WatchHook = WatchHook
  { -- | The shell command to execute. For example, @"stack build"@.
    command :: String,
    -- | How long to wait before running the command, in __microseconds__.
    -- A short delay (e.g., 100ms or `Just 100000`) is useful for ignoring
    -- rapid-fire events from a text editor. If 'Nothing', the command
    -- runs immediately.
    delay :: Maybe Int
  }
  deriving (Show)

-- | Internal helper to atomically swap the application's routes.
reloadRoutes :: MVar [Route] -> FilePath -> IO ()
reloadRoutes routesMVar gatewayPath = do
  putStrLn "Reloading routes..."
  newRoutes <- loadGatewayRoutes gatewayPath
  void $ swapMVar routesMVar newRoutes

-- | Watches a directory for file changes and runs a hook.
--
-- This function is the heart of the module. It solves the "event storm"
-- problem by using an 'MVar' as a gatekeeper.
--
-- === How it Works
--
-- 1. An event arrives.
-- 2. The watcher tries to grab a lock ('MVar').
-- 3. If it gets the lock, it starts a timer in a new thread ('forkIO').
-- 4. If it *doesn't* get the lock, it means another event is already being
-- handled, so it does nothing.
-- 5. After the timer finishes, the command runs and the lock is released,
-- ready for the next change.
--
-- This function is designed to be the main action of a program and will
-- run forever, so it will block the calling thread.
watchForChanges ::
  -- | The hook to run. If 'Nothing', this function does nothing.
  Maybe WatchHook ->
  -- | The directory path to watch for changes.
  FilePath ->
  -- | The path to the gateway definition file for route reloading.
  FilePath ->
  -- | The 'MVar' holding the server's routes to be updated.
  MVar [Route] ->
  IO ()
watchForChanges maybeHook watchDirectoryPath gatewayPath routesMVar = withManager $ \mgr -> do
  putStrLn $ "Watching for changes in: " ++ watchDirectoryPath

  case maybeHook of
    Nothing -> pure () -- If no hook is provided, we simply do nothing.
    Just WatchHook {..} -> do
      -- The MVar acts as a gatekeeper. When full, we can process an event.
      -- When empty, an event is already being processed, and newcomers are ignored.
      lock <- newMVar ()

      void $ watchTree mgr watchDirectoryPath (const True) $ \_event -> do
        -- tryTakeMVar is an atomic "check and grab" operation. This is the
        -- secret to preventing the race condition where multiple events
        -- are processed at once.
        wasLockAvailable <- tryTakeMVar lock

        -- Only proceed if we were the first to grab the lock.
        when (wasLockAvailable == Just ()) $ do
          -- forkIO runs the action in a new thread. This is critical.
          -- It prevents the delay from blocking the main event listener.
          void $ forkIO $ do
            let actualDelay = fromMaybe 0 delay
            putStrLn $ "Change detected. Waiting for " ++ show actualDelay ++ " microseconds..."
            threadDelay actualDelay

            putStrLn $ "Executing hook: " ++ command
            callCommand command
            reloadRoutes routesMVar gatewayPath

            putStrLn "Hook finished. Ready for next change."
            -- Releasing the lock allows the next event to be processed.
            putMVar lock ()

  -- Keep the main thread alive to allow the watcher to continue running.
  forever $ threadDelay maxBound