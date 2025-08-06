module Main (main) where

import Venusia.Server
import Venusia.Server.Watcher (watchForChanges, WatchHook(..))
import Venusia.Gateway (loadGatewayRoutes)
import Options.Applicative
import System.FilePath ((</>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)

-- | The name of the routes file to look for inside the watched directory.
gatewaysFile :: FilePath
gatewaysFile = "gateways.toml"

-- | The port the server will listen on.
serverPort :: String
serverPort = "7070"

-- | Defines the CLI's command structure.
data Command
  = Watch ServeWatchOptions

-- | Options for the 'watch' command.
data ServeWatchOptions = ServeWatchOptions
  { watchDir      :: FilePath
  , maybeHookInfo :: Maybe HookInfo -- CHANGED: Now holds optional hook info.
  }

-- | A new record to group the hook command and its delay.
data HookInfo = HookInfo
  { hookCommand :: String
  , hookDelay   :: Int -- Delay in milliseconds
  }

-- | Parser for the optional hook info block.
hookInfoParser :: Parser HookInfo
hookInfoParser = HookInfo
    <$> strArgument (metavar "CHANGE_HOOK" <> help "Shell command to run on change.")
    <*> argument auto (metavar "CHANGE_HOOK_DELAY" <> value 0 <> help "Optional delay in ms before running hook (ms).")

-- | Parser for the 'watch' command's arguments.
serveWatchCommand :: Parser Command
serveWatchCommand = Watch <$> (ServeWatchOptions
    <$> strArgument (metavar "WATCH_DIR" <> help "Directory to watch for changes.")
    <*> optional hookInfoParser) -- CHANGED: Use the new hook info parser.

-- | Main entry point
main :: IO () 
main = do
  let commands = subparser
        ( command "watch" (info serveWatchCommand (progDesc "Serve and watch a directory for changes. This directory will also be checked for gateways file."))
        )

  let opts = info (helper <*> commands)
               (fullDesc <> progDesc "Venusia Server and Systemd Utility")
  
  cmd <- execParser opts 
  runCommand cmd

-- | Dispatcher to run the logic for the chosen command.
runCommand :: Command -> IO ()
runCommand (Watch opts)   = runServeWatch opts

-- | The main action: starts the server and watches for changes in the specified directory.
runServeWatch :: ServeWatchOptions -> IO ()
runServeWatch ServeWatchOptions{..} = do
  let
    gatewayPath = watchDir </> gatewaysFile
    maybeWatchHook = fmap (\HookInfo{..} -> WatchHook hookCommand (Just hookDelay)) maybeHookInfo
  
  initialRoutes <- loadGatewayRoutes gatewayPath
  routesMVar <- newMVar initialRoutes
  _ <- forkIO $ watchForChanges maybeWatchHook watchDir gatewayPath routesMVar
  serveHotReload serverPort noMatchHandler routesMVar

