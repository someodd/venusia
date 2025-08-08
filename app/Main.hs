module Main (main) where

import Venusia.Server
import Venusia.Server.Watcher (watchForChanges, WatchHook(..))
import Venusia.Routes (loadRoutes)
import Options.Applicative
import System.FilePath ((</>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import qualified Data.Text as T

-- | The name of the routes file to look for inside the watched directory.
routesFile :: FilePath
routesFile = "routes.toml"

-- | Defines the CLI's command structure.
data Command
  = Watch ServeWatchOptions

-- | Options for the 'watch' command.
data ServeWatchOptions = ServeWatchOptions
  { watchDir      :: FilePath
  , host          :: String
  , port          :: Int
  , maybeHookInfo :: Maybe HookInfo
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
    <*> strArgument (metavar "HOST" <> help "Host to bind the server to (e.g., '127.0.0.1' or 'gopher.someodd.zip').")
    <*> argument auto (metavar "PORT" <> help "Port number to listen on (e.g., 7070).")
    <*> optional hookInfoParser)

-- | Main entry point
main :: IO ()
main = do
  let commands = subparser
        ( command "watch" (info serveWatchCommand (progDesc "Serve and watch a directory for changes. This directory will also be checked for a routes.toml file."))
        )

  let opts = info (helper <*> commands)
               (fullDesc <> progDesc "Venusia Internet Gopher Protocol Server Daemon")

  cmd <- execParser opts
  runCommand cmd

-- | Dispatcher to run the logic for the chosen command.
runCommand :: Command -> IO ()
runCommand (Watch opts)   = runServeWatch opts

-- | The main action: starts the server and watches for changes in the specified directory.
runServeWatch :: ServeWatchOptions -> IO ()
runServeWatch ServeWatchOptions{..} = do
  let
    routesPath = watchDir </> routesFile
    maybeWatchHook = fmap (\HookInfo{..} -> WatchHook hookCommand (Just hookDelay)) maybeHookInfo
    hostText = T.pack host

  initialRoutes <- loadRoutes routesPath hostText port
  routesMVar <- newMVar initialRoutes
  _ <- forkIO $ watchForChanges hostText port maybeWatchHook watchDir routesPath routesMVar
  serveHotReload (show port) noMatchHandler routesMVar
