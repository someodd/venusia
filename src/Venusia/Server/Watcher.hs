module Venusia.Server.Watcher (watchForChanges, WatchHook (..)) where

import Control.Concurrent (MVar, forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, swapMVar)
import Control.Monad (forever, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.FSNotify (Event (..), watchTree, withManager)
import System.FilePath ((</>))
import System.Process (callCommand)
import Venusia.Gateway (loadGatewayRoutes)
import Venusia.Server (Route)

-- | Command that executes upon changes in the watched directory.
data WatchHook = WatchHook
  { command :: String,
    -- | Delay in milliseconds
    delay :: Maybe Int
  }

-- | Watch for changes in a directory, and reload routes accordingly
-- If a hook command is provided, it will be executed after the routes are reloaded.
-- The hook command will only be executed if the specified delay has passed since the last change.
-- It's recommended to specify a delay to avoid rapid reloading.
watchForChanges :: Maybe WatchHook -> FilePath -> FilePath -> MVar [Route] -> IO ()
watchForChanges maybeHook watchDirectoryPath gatewayPath routesMVar = withManager $ \mgr -> do
  putStrLn $ "Watching for changes in: " ++ watchDirectoryPath
  -- Create a mutable reference to track when the last rebuild was triggered
  lastRebuildVar <- newIORef =<< getCurrentTime

  void $ watchTree mgr watchDirectoryPath (const True) $ \_ -> do
    -- Logic to deal with the optional hook.
    case maybeHook of
      Nothing -> pure ()
      Just WatchHook {..} -> do
        -- Check if enough time has passed since the last rebuild. only applies if hook time is specified.
        now <- getCurrentTime
        lastRebuild <- readIORef lastRebuildVar
        let microsecondsSinceLastRebuild = floor $ (realToFrac $ diffUTCTime now lastRebuild :: Float) * 1000000
            hookDelay = fromMaybe 0 delay
        -- Has the delay elapsed?
        when (microsecondsSinceLastRebuild >= hookDelay) $ do
          writeIORef lastRebuildVar =<< getCurrentTime
          putStrLn $ "Waiting " ++ show hookDelay ++ " seconds before executing hook..."
          threadDelay hookDelay

        putStrLn $ "Executing hook: " ++ command
        callCommand command

    -- Load "new" route configuration state into the mvar
    putStrLn $ "Change detected in " ++ watchDirectoryPath ++ ", reloading routes from " ++ gatewayPath
    newRoutes <- loadGatewayRoutes gatewayPath
    swapMVar routesMVar newRoutes
    putStrLn "Routes reloaded."

  forever $ threadDelay maxBound