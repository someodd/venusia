{- | This is merely an example!
-}
module Main (main) where

import Venusia.Server
import Venusia.MenuBuilder
import Venusia.FileHandler
import Venusia.Gateway (executeProcessWithArgs, loadGatewayRoutes)
import Venusia.Systemd
import qualified Data.Text as T
import System.Environment (getArgs, getExecutablePath)


host :: T.Text
host = "localhost"

port :: Int
port = 7070

-- | Register your routes.
routes :: [Route]
routes =
  [ on "/hello" $ \_ ->
      return $ TextResponse "Hello, gopher!\r\n"
  , onWildcard "/echo/*/something" $ \request ->
      case request.reqWildcard of
        Just wildcard -> pure $ TextResponse wildcard
        Nothing -> pure $ TextResponse "Nothing."
  , on "/search" handleSearch
  , on "/cowsay" handleCowsay
  , on "/ollama" handleOllama
  , on "/weather" handleWeather
  , onWildcard "/figlet/*" handleFiglet
  , onWildcard "/echo-process/*" handleEcho
  , onWildcard "/files/*" $ \request ->
      case request.reqWildcard of
        Just wildcard -> serveDirectory host port "/home/tilde" "/files/" wildcard Nothing
        Nothing -> pure $ TextResponse "No wildcard provided."
  , onWildcard "/superSearch/*/bar" handleWildcardSearch
  ]

-- | Handler for the wildcard search route.
handleWildcardSearch :: Request -> IO Response
handleWildcardSearch request =
  case (request.reqWildcard, request.reqQuery) of
    (Nothing, _) ->
      pure $ TextResponse $ info "Venusia was coded incorrectly, apparently."
    (_, Nothing) ->
      pure $ TextResponse $ info "User error: missing query."
    (Just wildcard, Just query) -> do
      pure . TextResponse . info $ wildcard <> " AND " <> query

-- | Handler for search queries (Gopher item type 7).
handleSearch :: Request -> IO Response
handleSearch request = do
  let query =
        case request.reqQuery of
          Nothing -> ""
          (Just something) -> something
  -- Build the response
  return . TextResponse . render $
    [ info "Search results for: " <> query
    , text "Example file" "/fake" host port
    , directory "Example dir" "/fake" host port
    ]

handleEcho :: Request -> IO Response
handleEcho request =
  case request.reqWildcard of
    Nothing ->
      pure $ TextResponse "No wildcard provided."
    (Just something) -> do
      -- Execute the cowsay command with the query as an argument
      executeProcessWithArgs "echo" [T.unpack something] False

handleCowsay :: Request -> IO Response
handleCowsay request =
  case request.reqQuery of
    Nothing ->
      pure $ TextResponse "No query provided."
    (Just something) -> do
      -- Execute the cowsay command with the query as an argument
      executeProcessWithArgs "cowsay" [T.unpack something] True

handleWeather :: Request -> IO Response
handleWeather request = do
  case request.reqQuery of
    Nothing ->
      pure $ TextResponse "No city provided."
    (Just city) -> do
      let safeCity = T.replace " " "%20" city
      executeProcessWithArgs "curl" ["https://wttr.in/" <> T.unpack safeCity <> "?format=3"] True

handleOllama :: Request -> IO Response
handleOllama request =
  case request.reqQuery of
    Nothing ->
      pure $ TextResponse "No query provided."
    (Just query) -> do
      executeProcessWithArgs "ollama" ["run", "llama3:latest", T.unpack query] True

handleFiglet :: Request -> IO Response
handleFiglet request = do
  case request.reqQuery of
    Nothing ->
      pure $ TextResponse "No query provided."
    (Just textToRender) -> do
      case request.reqWildcard of
        Just fontToUse ->
          executeProcessWithArgs "figlet" [T.unpack fontToUse, T.unpack textToRender] True
        Nothing -> do
          pure $ TextResponse "No font/wildcard provided."

-- FIXME: add wildcard example for process, provide text file (False)

-- | Main entry point
main :: IO ()
main = do
  args <- getArgs
  
  -- Check if --setup-service flag is present
  if "--setup-service" `elem` args then
    setupService
  else do
    gatewayRoutes <- loadGatewayRoutes "gateways.toml"
    serve "7070" noMatchHandler (gatewayRoutes ++ routes)
    
  where
    -- Setup service for Debian
    setupService = do
      -- Get the path to our own executable
      exePath <- getExecutablePath
      
      -- Setup the systemd service
      setupSystemdService 
        "venusia-demo"  -- Service name
        exePath           -- Executable path
        "7070"              -- Port
        Nothing   -- User
        Nothing   -- Group
        (Just "/tmp/")  -- Working directory