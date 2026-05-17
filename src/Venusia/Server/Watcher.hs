-- | File watcher with debouncing and an optional pre-reload hook.
--
-- Each event arrives, the watcher tries to grab an MVar lock, and if it
-- gets it forks a thread that sleeps the debounce delay, runs the hook
-- (if any), reloads routes, and releases the lock. Subsequent events
-- arriving while the lock is held are silently dropped — the
-- first-edge-wins debounce that prevents a save storm from triggering
-- a storm of hooks.
--
-- Resilience invariants (added to fix the long-standing silent-failure
-- mode where Bartleby would stop being triggered):
--
--   * The watch is registered whether or not a hook is configured. A
--     'routes.toml' edit reloads routes regardless; the hook is just
--     an optional pre-reload step.
--   * The forked handler is wrapped in 'finally' so the lock is
--     released no matter what — a failing hook or a malformed
--     'routes.toml' can't deadlock the watcher.
--   * Both the hook invocation and 'reloadRoutes' are wrapped in
--     'try' so exceptions land as log lines, not silent thread death.
--   * Startup and per-event log lines exist so an operator can tell,
--     from 'journalctl' alone, whether the watch is alive and
--     whether events are arriving.
module Venusia.Server.Watcher (watchForChanges, WatchHook (..)) where

import qualified Data.Text as T
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, swapMVar, tryTakeMVar)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forever, void, when)
import Data.Maybe (fromMaybe)
import System.FSNotify (watchTree, withManager)
import System.Process (callCommand)
import Venusia.Routes (loadRoutes)
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
reloadRoutes :: MVar [Route] -> FilePath -> T.Text -> Int -> IO ()
reloadRoutes routesMVar gatewayPath host port = do
  putStrLn "Reloading routes..."
  newRoutes <- loadRoutes gatewayPath host port
  void $ swapMVar routesMVar newRoutes

-- | Run the hook (if any) then reload routes. Each step is wrapped in
-- 'try' so one failure doesn't prevent the next; both failures are
-- logged. Caller is responsible for the surrounding lock discipline.
handleChange
  :: Maybe WatchHook -> MVar [Route] -> FilePath -> T.Text -> Int -> IO ()
handleChange maybeHook routesMVar gatewayPath host port = do
  threadDelay actualDelay
  case maybeHook of
    Nothing -> pure ()
    Just WatchHook{..} -> do
      putStrLn $ "Executing hook: " ++ command
      result <- try (callCommand command)
      case (result :: Either SomeException ()) of
        Right () -> putStrLn "Hook finished."
        Left  e  -> putStrLn $ "Hook FAILED (continuing with reload): " ++ show e
  reloadResult <- try (reloadRoutes routesMVar gatewayPath host port)
  case (reloadResult :: Either SomeException ()) of
    Right () -> putStrLn "Ready for next change."
    Left  e  -> putStrLn $ "Reload FAILED: " ++ show e
  where
    actualDelay = case maybeHook of
      Nothing -> 0
      Just h  -> fromMaybe 0 h.delay

-- | Watches a directory for file changes and (optionally) runs a hook
-- before reloading routes. Registers the watch whether or not a hook
-- is configured — 'routes.toml' edits trigger a reload either way.
--
-- This function blocks forever; run it in its own thread.
watchForChanges ::
  -- | The host to use for relative/local menu items.
  T.Text ->
  -- | The port to use for relative/local menu items.
  Int ->
  -- | The hook to run after a change, before the routes reload.
  -- 'Nothing' means just reload routes.
  Maybe WatchHook ->
  -- | The directory path to watch for changes.
  FilePath ->
  -- | The path to the routes definition file for route reloading.
  FilePath ->
  -- | The 'MVar' holding the server's routes to be updated.
  MVar [Route] ->
  IO ()
watchForChanges host port maybeHook watchDirectoryPath gatewayPath routesMVar = withManager $ \mgr -> do
  putStrLn $ "Watching for changes in: " ++ watchDirectoryPath

  -- The MVar acts as a gatekeeper. When full, we can process an event.
  -- When empty, an event is already being processed and newcomers are
  -- silently dropped (first-edge debounce).
  lock <- newMVar ()

  void $ watchTree mgr watchDirectoryPath (const True) $ \event -> do
    putStrLn $ "fsnotify event: " ++ show event
    wasLockAvailable <- tryTakeMVar lock
    when (wasLockAvailable == Just ()) $
      -- forkIO so the debounce delay doesn't block the event listener.
      -- 'finally' guarantees the lock is released even if 'handleChange'
      -- throws — the original code would leave the MVar empty forever
      -- after any hook failure, which silently disabled the watcher.
      void $ forkIO $
        handleChange maybeHook routesMVar gatewayPath host port
          `finally` putMVar lock ()

  putStrLn $ "Watch registered on " ++ watchDirectoryPath

  -- Keep the main thread alive to allow the watcher to continue running.
  forever $ threadDelay maxBound
