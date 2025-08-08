{- | Gopher Route Configuration

This module handles parsing a routes.toml file to configure and build all
server routes, including command gateways, file servers, and search handlers.
-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}

module Venusia.Routes
  ( loadRoutes
  ) where

import           Control.Exception          (SomeException, try)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Maybe                 (fromMaybe)
import qualified System.Process             as P
import           System.IO.Error            (catchIOError)
import           GHC.Generics               (Generic)
import           Control.Monad              (forM_)


import qualified Toml
import           Toml                       (TomlCodec, (.=))

import           Venusia.MenuBuilder        (error', info, render)
import           Venusia.Server             (Handler, Request (..), Response (..),
                                             Route, on, onWildcard)
import           Venusia.FileHandler        (serveDirectory)
import           Venusia.SearchHandler      (serveSearch)

-- Configuration Data Types ---

-- | Top-level configuration holding lists for each route type.
data RoutesConfig = RoutesConfig
  { gateways :: [GatewayConfig] -- ^ List of command gateway configurations
  , searches :: [SearchConfig]  -- ^ List of search handler configurations
  , files    :: [FilesConfig]   -- ^ List of file server configurations
  } deriving (Show, Eq, Generic)

-- | Configuration for a command-based gateway process.
data GatewayConfig = GatewayConfig
  { selector  :: T.Text
  , command   :: T.Text
  , arguments :: [T.Text]
  , wildcard  :: Bool
  , menu      :: Bool
  , preamble  :: Maybe [T.Text]
  , postamble :: Maybe [T.Text]
  } deriving (Show, Eq, Generic)

-- | Configuration for a built-in search handler.
data SearchConfig = SearchConfig
  { selector :: T.Text
  , path     :: FilePath
  } deriving (Show, Eq, Generic)

-- | Configuration for a built-in file server.
data FilesConfig = FilesConfig
  { selector :: T.Text
  , path     :: FilePath
  } deriving (Show, Eq, Generic)


-- TOML Parsing Codecs ---

-- | Codec for the top-level RoutesConfig.
-- This was the main source of the first error. Toml.genericCodec can't be used
-- on the top-level type, so we build it from the individual list codecs.
routesConfigCodec :: TomlCodec RoutesConfig
routesConfigCodec = RoutesConfig
    <$> Toml.list gatewayConfigCodec "gateway" .= (.gateways)
    <*> Toml.list searchConfigCodec  "search"  .= (.searches)
    <*> Toml.list filesConfigCodec   "files"   .= (.files)

-- | Codec for a single command gateway configuration.
gatewayConfigCodec :: TomlCodec GatewayConfig
gatewayConfigCodec = Toml.genericCodec

-- | Codec for a single search handler configuration.
searchConfigCodec :: TomlCodec SearchConfig
searchConfigCodec = SearchConfig
    <$> Toml.text "selector" .= (.selector)
    <*> Toml.string "path"   .= (.path)

-- | Codec for a single file server configuration.
filesConfigCodec :: TomlCodec FilesConfig
filesConfigCodec = FilesConfig
    <$> Toml.text "selector" .= (.selector)
    <*> Toml.string "path"   .= (.path)


-- Route Loading and Building ---

-- | Reads the TOML config and builds a list of all Gopher routes.
loadRoutes :: FilePath -> T.Text -> Int -> IO [Route]
loadRoutes path host port = do
  eConfig <- readRoutesConfig path
  case eConfig of
    Left err -> do
      putStrLn $ "Error loading route configuration: " ++ err
      putStrLn "No routes loaded."
      return []
    Right config -> do
      let routes = buildRoutes config host port
      putStrLn $ "Successfully loaded " ++ show (length routes) ++ " routes."
      return routes

-- | Reads and decodes the routes configuration from a TOML file.
readRoutesConfig :: FilePath -> IO (Either String RoutesConfig)
readRoutesConfig path = catchIOError
  (do
    contents <- TIO.readFile path
    case Toml.decode routesConfigCodec contents of
      Left err      -> pure . Left $ "Failed to parse TOML: " ++ show err
      Right config -> do
        -- Optional: Log what was loaded
        forM_ (config.gateways) $ \c -> putStrLn $ "Loaded gateway: " ++ T.unpack c.selector
        forM_ (config.searches) $ \c -> putStrLn $ "Loaded search: " ++ T.unpack c.selector
        forM_ (config.files)    $ \c -> putStrLn $ "Loaded files: " ++ T.unpack c.selector
        pure $ Right config
  )
  (\e -> pure . Left $ "Error reading file: " ++ show e)

-- | Builds a list of routes from the parsed RoutesConfig.
-- This function was fixed to handle all three route types and pass parameters correctly.
buildRoutes :: RoutesConfig -> T.Text -> Int -> [Route]
buildRoutes config host port =
    (buildGatewayRoutes $ config.gateways) ++
    (buildSearchRoutes  (config.searches) host port) ++
    (buildFileRoutes    (config.files)    host port)

-- | Builds routes for command gateways.
buildGatewayRoutes :: [GatewayConfig] -> [Route]
buildGatewayRoutes = concatMap createGatewayRoute
  where
    createGatewayRoute config =
      let handler = createCommandHandler config
      in if config.wildcard
          then [onWildcard config.selector handler]
          else [on config.selector handler]

-- | Builds routes for search handlers.
buildSearchRoutes :: [SearchConfig] -> T.Text -> Int -> [Route]
buildSearchRoutes configs host port = concatMap (createSearchRoute host port) configs
  where
    createSearchRoute host port config =
      [on config.selector (createSearchHandler config host port)]

-- | Builds routes for file servers.
buildFileRoutes :: [FilesConfig] -> T.Text -> Int -> [Route]
buildFileRoutes configs host port = concatMap (createFileRoute host port) configs
  where
    createFileRoute host port config =
      [onWildcard config.selector (createFileHandler config host port)]


-- Handler Creation ---

-- | Creates a handler for a file server configuration.
createFileHandler :: FilesConfig -> T.Text -> Int -> Handler
createFileHandler config host port request =
  case request.reqWildcard of
    Just wildcard -> serveDirectory host port config.path config.selector wildcard Nothing
    Nothing       -> pure $ TextResponse "Error: No path provided for file handler."

-- | Creates a handler for a search handler configuration.
createSearchHandler :: SearchConfig -> T.Text -> Int -> Handler
createSearchHandler config host port request =
  let query = fromMaybe "" request.reqQuery
  in serveSearch config.path host port query

-- | Creates a handler for a command gateway configuration.
createCommandHandler :: GatewayConfig -> Handler
createCommandHandler config request =
  let
    args = config.arguments
    processedArgs = map (substituteArgPlaceholders request.reqQuery request.reqWildcard . T.unpack) args
    -- A search is implied if the $search placeholder is used.
    isSearch = any (T.isInfixOf "$search") args
    canExecute = not (isSearch && request.reqQuery == Nothing)
  in
    if canExecute
      then executeProcessWithArgs config processedArgs
      else pure $ TextResponse $ error' "A search query is required for this gateway."


-- Process Execution and Helpers ---

-- | Substitute placeholders like $search and $wildcard in command arguments.
substituteArgPlaceholders :: Maybe T.Text -> Maybe T.Text -> String -> String
substituteArgPlaceholders searchValue wildcardValue =
    T.unpack . T.replace "$wildcard" (fromMaybe "" wildcardValue)
             . T.replace "$search" (fromMaybe "" searchValue)
             . T.pack

-- | Execute a process with arguments and format the response.
executeProcessWithArgs :: GatewayConfig -> [String] -> IO Response
executeProcessWithArgs config args = do
    result <- try (P.readProcess (T.unpack config.command) args "") :: IO (Either SomeException String)
    let contents = case result of
          Left err  -> [error' . T.pack $ "Process execution failed: " ++ show err]
          Right out -> if config.menu
                         then info . T.pack <$> lines out
                         else [T.pack out]
    pure . TextResponse . render $ fromMaybe [] config.preamble ++ contents ++ fromMaybe [] config.postamble
