{- | Gopher Gateway/connect to processes.

Allow a request to be sent to a process and return the response.
Supports executing processes with or without arguments.
Includes proper error handling for process execution.

Also includes tools for parsing a gateways.toml file.
-}

module Venusia.Gateway 
  ( executeProcess
  , executeProcessWithArgs
  , readGatewaysConfig
  , buildGatewayRoutes
  , loadGatewayRoutes
  , GatewayConfig(..)
  ) where

import Venusia.Server (Response(..), Route, Request(..), on, onWildcard, Handler)
import qualified System.Process as P
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Venusia.MenuBuilder (error', info)
import qualified Toml
import Toml (TomlCodec, (.=))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import System.IO.Error (catchIOError)
import Control.Monad (forM, forM_)
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map

-- | Configuration for a gateway process
data GatewayConfig = GatewayConfig
  { gatewaySelector :: T.Text      -- ^ The selector path
  , gatewaySearch   :: Bool        -- ^ Whether this is a search gateway
  , gatewayWildcard :: Bool        -- ^ Whether this uses wildcard matching
  , gatewayCommand  :: T.Text      -- ^ The command to execute
  , gatewayArgs     :: [T.Text]    -- ^ Arguments to pass to the command
  , gatewayMenu     :: Bool        -- ^ Whether to format the response as a menu
  } deriving (Show, Eq)

-- | Codec for a single gateway configuration
gatewayConfigCodec :: TomlCodec GatewayConfig
gatewayConfigCodec = GatewayConfig
  <$> Toml.text "selector" .= gatewaySelector
  <*> Toml.bool "search" .= gatewaySearch
  <*> Toml.bool "wildcard" .= gatewayWildcard
  <*> Toml.text "command" .= gatewayCommand
  <*> Toml.arrayOf Toml._Text "arguments" .= gatewayArgs
  <*> Toml.bool "menu" .= gatewayMenu

-- | Data type for the top-level configuration that matches the TOML structure
data GatewaysConfig = GatewaysConfig 
  { gatewaysCowsay      :: Maybe GatewayConfig
  , gatewaysOllama      :: Maybe GatewayConfig
  , gatewaysWeather     :: Maybe GatewayConfig
  , gatewaysFiglet      :: Maybe GatewayConfig
  , gatewaysEchoProcess :: Maybe GatewayConfig
  } deriving (Show, Eq)

-- | Codec for the overall gateways configuration
gatewaysCodec :: TomlCodec GatewaysConfig
gatewaysCodec = GatewaysConfig
  <$> Toml.dioptional (Toml.table gatewayConfigCodec "gateways.cowsay") .= gatewaysCowsay
  <*> Toml.dioptional (Toml.table gatewayConfigCodec "gateways.ollama") .= gatewaysOllama
  <*> Toml.dioptional (Toml.table gatewayConfigCodec "gateways.weather") .= gatewaysWeather
  <*> Toml.dioptional (Toml.table gatewayConfigCodec "gateways.figlet") .= gatewaysFiglet
  <*> Toml.dioptional (Toml.table gatewayConfigCodec "gateways.echo-process") .= gatewaysEchoProcess

-- | Read the gateways configuration from a TOML file
readGatewaysConfig :: FilePath -> IO (Either String (HM.HashMap T.Text GatewayConfig))
readGatewaysConfig path = catchIOError
  (do
    putStrLn $ "Attempting to read TOML file: " ++ path
    contents <- TIO.readFile path
    putStrLn $ "TOML file read successfully, parsing..."
    
    case Toml.decode gatewaysCodec contents of
      Left err -> do
        putStrLn $ "Failed to parse TOML: " ++ show err
        return $ Left $ "Failed to parse TOML: " ++ show err
      Right config -> do
        -- Convert the config to a HashMap
        let gatewaysMap = convertToHashMap config
        putStrLn $ "Successfully parsed " ++ show (HM.size gatewaysMap) ++ " gateways"
        forM_ (HM.toList gatewaysMap) $ \(name, config) -> 
          putStrLn $ "  Gateway: " ++ T.unpack name ++ " -> " ++ T.unpack (gatewaySelector config)
        
        return $ Right gatewaysMap
  )
  (\e -> do
    putStrLn $ "Error reading file: " ++ show e
    return $ Left $ "Error reading file: " ++ show e
  )

-- | Convert the GatewaysConfig to a HashMap
convertToHashMap :: GatewaysConfig -> HM.HashMap T.Text GatewayConfig
convertToHashMap config = HM.fromList $ catMaybes
  [ fmap ("cowsay",) (gatewaysCowsay config)
  , fmap ("ollama",) (gatewaysOllama config)
  , fmap ("weather",) (gatewaysWeather config)
  , fmap ("figlet",) (gatewaysFiglet config)
  , fmap ("echo-process",) (gatewaysEchoProcess config)
  ]

-- | Filter out Nothing values from a list
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

-- | Load gateway routes from a TOML configuration file
loadGatewayRoutes :: FilePath -> IO [Route]
loadGatewayRoutes path = do
  result <- readGatewaysConfig path
  case result of
    Left err -> do
      putStrLn $ "Error loading gateway configuration: " ++ err
      putStrLn "No gateway routes loaded."
      return []
    Right gateways -> do
      let routes = buildGatewayRoutes gateways
      putStrLn $ "Successfully loaded " ++ show (length routes) ++ " gateway routes"
      return routes

-- | Create a handler for a gateway configuration
createGatewayHandler :: GatewayConfig -> Handler
createGatewayHandler config = \request ->
  let 
    searchValue = request.reqQuery
    wildcardValue = request.reqWildcard
    
    -- Replace placeholders in arguments with actual values
    processedArgs = map (substituteArgPlaceholders searchValue wildcardValue) (map T.unpack config.gatewayArgs)
    
    -- Check if we can execute the command based on available values
    canExecute = case (config.gatewaySearch, config.gatewayWildcard, searchValue, wildcardValue) of
      -- Search required but not provided
      (True, _, Nothing, _) -> 
        not (any containsSearchPlaceholder config.gatewayArgs)
      
      -- Wildcard required but not provided
      (_, True, _, Nothing) -> 
        not (any containsWildcardPlaceholder config.gatewayArgs)
        
      -- Otherwise we can execute
      _ -> True
  in
    if canExecute
      then executeProcessWithArgs (T.unpack config.gatewayCommand) processedArgs config.gatewayMenu
      else if config.gatewaySearch && isNothing searchValue
        then return $ TextResponse $ error' "No search query provided."
        else if config.gatewayWildcard && isNothing wildcardValue
          then return $ TextResponse $ error' "No wildcard value captured."
          else return $ TextResponse $ error' "Missing required arguments."

-- | Check if text contains search placeholder
containsSearchPlaceholder :: T.Text -> Bool
containsSearchPlaceholder = T.isInfixOf "$search"

-- | Check if text contains wildcard placeholder
containsWildcardPlaceholder :: T.Text -> Bool
containsWildcardPlaceholder = T.isInfixOf "$wildcard"

-- | Check if Maybe value is Nothing
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- | Substitute argument placeholders with actual values
substituteArgPlaceholders :: Maybe T.Text -> Maybe T.Text -> String -> String
substituteArgPlaceholders searchValue wildcardValue arg =
  let
    -- Replace $search with search value if available
    withSearch = case searchValue of
      Just search -> replace "$search" (T.unpack search) arg
      Nothing     -> replace "$search" "" arg
    
    -- Replace $wildcard with wildcard value if available
    withBoth = case wildcardValue of
      Just wildcard -> replace "$wildcard" (T.unpack wildcard) withSearch
      Nothing       -> replace "$wildcard" "" withSearch
    
    -- For backward compatibility, also handle %s
    withLegacy = case searchValue of
      Just search -> replace "%s" (T.unpack search) withBoth
      Nothing     -> case wildcardValue of
        Just wildcard -> replace "%s" (T.unpack wildcard) withBoth
        Nothing       -> withBoth
  in
    withLegacy

-- | Replace all occurrences of a substring in a string
replace :: String -> String -> String -> String
replace old new "" = ""
replace old new str@(c:cs) =
  case stripPrefix old str of
    Just rest -> new ++ replace old new rest
    Nothing   -> c : replace old new cs

-- | Check if a string has a given prefix and return the remainder
stripPrefix :: String -> String -> Maybe String
stripPrefix "" ys = Just ys
stripPrefix (x:xs) (y:ys) = if x == y then stripPrefix xs ys else Nothing
stripPrefix _ _ = Nothing

-- | Build routes from a map of gateway configurations
buildGatewayRoutes :: HM.HashMap T.Text GatewayConfig -> [Route]
buildGatewayRoutes gateways = 
  concatMap createRoute (HM.elems gateways)
  where
    createRoute :: GatewayConfig -> [Route]
    createRoute config = 
      -- Create appropriate route based on configuration
      if config.gatewayWildcard
        -- If wildcard is enabled, create a wildcard route
        then [onWildcard (config.gatewaySelector <> "/*") (createGatewayHandler config)]
        -- Otherwise create a standard route
        -- Note: Search capability is handled in the handler, not in route creation
        else [on config.gatewaySelector (createGatewayHandler config)]

-- | Execute a process without arguments and return the response.
executeProcess :: String -> IO Response
executeProcess command = do
    result <- try (P.readProcess command [] "") :: IO (Either SomeException String)
    case result of
        Left err  -> return $ TextResponse $ T.pack $ "Process execution failed: " ++ show err
        Right out -> return $ TextResponse $ T.pack out

-- | Execute a process with arguments and return the response.
--
-- You NEED to use `asMenu` for a proper response to search queries, I believe this is
-- part of the gopher spec, or at least the client I tested expects a menu as a search
-- response.
executeProcessWithArgs :: String -> [String] -> Bool -> IO Response
executeProcessWithArgs command args asMenu = do
    result <- try (P.readProcess command args "") :: IO (Either SomeException String)
    case result of
        Left err  ->
            if asMenu
                then return $ TextResponse $ error' . T.pack $ "Process execution failed: " ++ show err
                else return $ TextResponse $ T.pack $ "Process execution failed: " ++ show err
        Right out ->
            if asMenu
                then
                    return $ TextResponse . T.unlines $ info . T.pack <$> lines out
                else
                    return $ TextResponse $ T.pack out