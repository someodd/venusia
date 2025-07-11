module Main (main) where

import Venusia.Server
import Venusia.Server.Watcher (watchForChanges, WatchHook(..))
import Venusia.Gateway (loadGatewayRoutes)
import Venusia.Systemd
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
  | Systemd SystemdOptions

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

-- | Options for the 'systemd' command.
data SystemdOptions = SystemdOptions
  { executablePath :: FilePath
  , port           :: String
  , serviceUser    :: Maybe String
  , serviceGroup   :: Maybe String
  , workingDir     :: Maybe FilePath
  }

-- | Parser for the 'systemd' command.
systemdCommand :: Parser Command
systemdCommand = fmap Systemd $ SystemdOptions
    <$> strArgument (metavar "EXECUTABLE_PATH" <> help "Absolute path to the service executable.")
    <*> strOption (long "port" <> metavar "PORT" <> value serverPort <> help "Port for the service.")
    <*> optional (strOption (long "user" <> metavar "USER" <> help "User for the service."))
    <*> optional (strOption (long "group" <> metavar "GROUP" <> help "Group for the service."))
    <*> optional (strOption (long "work-dir" <> metavar "WORKING_DIRECTORY" <> help "Working directory for the service."))

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
        ( command "watch" (info serveWatchCommand (progDesc "Serve and watch a directory for changes."))
       <> command "systemd" (info systemdCommand (progDesc "Setup a systemd service."))
        )

  let opts = info (helper <*> commands)
               (fullDesc <> progDesc "Venusia Server and Systemd Utility")
  
  cmd <- execParser opts
  runCommand cmd

-- | Dispatcher to run the logic for the chosen command.
runCommand :: Command -> IO ()
runCommand (Watch opts)   = runServeWatch opts
runCommand (Systemd opts) = runSystemd opts 

-- | The main action: starts the server and watches for changes in the specified directory.
runServeWatch :: ServeWatchOptions -> IO ()
runServeWatch opts@ServeWatchOptions{..} = do
  let
    gatewayPath = watchDir </> gatewaysFile
    maybeWatchHook = fmap (\HookInfo{..} -> WatchHook hookCommand (Just hookDelay)) maybeHookInfo
  
  initialRoutes <- loadGatewayRoutes gatewayPath
  routesMVar <- newMVar initialRoutes
  _ <- forkIO $ watchForChanges maybeWatchHook watchDir gatewayPath routesMVar
  serveHotReload serverPort noMatchHandler routesMVar

-- | Sets up the systemd service file.
runSystemd :: SystemdOptions -> IO ()
runSystemd SystemdOptions {..} = do
  putStrLn "Setting up systemd service..."
  setupSystemdService "venusia-demo" executablePath port serviceUser serviceGroup workingDir
  putStrLn "Systemd service setup complete."

