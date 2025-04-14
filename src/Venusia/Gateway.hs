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
import Venusia.MenuBuilder (error', info, render)
import qualified Toml
import Toml (TomlCodec, (.=))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, catMaybes)
import System.IO.Error (catchIOError)
import Control.Monad (forM, forM_)
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

-- | Configuration for a gateway process
data GatewayConfig = GatewayConfig
  { selector :: T.Text      -- ^ The selector path
  , search   :: Bool        -- ^ Whether this is a search gateway
  , wildcard :: Bool        -- ^ Whether this uses wildcard matching
  , command  :: T.Text      -- ^ The command to execute
  , arguments     :: [T.Text]    -- ^ Arguments to pass to the command
  , menu     :: Bool        -- ^ Whether to format the response as a menu
  , preamble :: Maybe [T.Text]    -- ^ Preamble text to include in the response
  , postamble :: Maybe [T.Text]   -- ^ Postamble text to include in the response
  } deriving (Show, Eq, Generic)

data Gateways = Gateways
  { gateways :: ![GatewayConfig] -- ^ List of gateway configurations
  } deriving (Show, Eq, Generic)

-- | Codec for a single gateway configuration using Generic
gatewaysCodec :: TomlCodec [GatewayConfig]
gatewaysCodec = Toml.list gatewayConfigCodec "gateway"

-- | Codec for a single gateway configuration using Generic
gatewayConfigCodec :: TomlCodec GatewayConfig
gatewayConfigCodec = Toml.genericCodec

-- | Read the gateways configuration from a TOML file
readGatewaysConfig :: FilePath -> IO (Either String [GatewayConfig])
readGatewaysConfig path = catchIOError
  (do
    putStrLn $ "Attempting to read TOML file: " ++ path
    contents <- TIO.readFile path
    putStrLn $ "TOML file read successfully, parsing..."
    
    -- Parse the TOML using our defined codec
    case Toml.decode gatewaysCodec contents of
      Left err -> do
        putStrLn $ "Failed to parse TOML: " ++ show err
        return $ Left $ "Failed to parse TOML: " ++ show err
      
      Right gateways -> do
        putStrLn $ "Successfully parsed " ++ show (length gateways) ++ " gateways"
        forM_ gateways $ \config -> 
          putStrLn $ "  Gateway: " ++ T.unpack config.selector
        
        return $ Right gateways
  )
  (\e -> do
    putStrLn $ "Error reading file: " ++ show e
    return $ Left $ "Error reading file: " ++ show e
  )

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
createGatewayHandler config request =
  let 
    searchValue = reqQuery request
    wildcardValue = reqWildcard request
    
    -- Replace placeholders in arguments with actual values
    processedArgs = map (substituteArgPlaceholders searchValue wildcardValue) (map T.unpack (arguments config))
    
    -- Check if we can execute the command based on available values
    canExecute = case (search config, wildcard config, searchValue, wildcardValue) of
      -- Search required but not provided
      (True, _, Nothing, _) -> 
        not (any containsSearchPlaceholder (arguments config))
      
      -- Wildcard required but not provided
      (_, True, _, Nothing) -> 
        not (any containsWildcardPlaceholder (arguments config))
        
      -- Otherwise we can execute
      _ -> True
  in
    if canExecute
      then executeProcessWithArgs 
             (T.unpack (command config)) 
             processedArgs 
             (menu config) 
             (fromMaybe [] config.preamble)
             (fromMaybe [] config.postamble)
      else if search config && isNothing searchValue
        then return $ TextResponse $ error' "No search query provided."
        else if wildcard config && isNothing wildcardValue
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
buildGatewayRoutes :: [GatewayConfig] -> [Route]
buildGatewayRoutes gateways = 
  concatMap createRoute gateways
  where
    createRoute :: GatewayConfig -> [Route]
    createRoute config = 
      -- Create appropriate route based on configuration
      if wildcard config
        -- If wildcard is enabled, create a wildcard route
        then [onWildcard (selector config) (createGatewayHandler config)]
        -- Otherwise create a standard route
        -- Note: Search capability is handled in the handler, not in route creation
        else [on (selector config) (createGatewayHandler config)]

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
executeProcessWithArgs :: String -> [String] -> Bool -> [T.Text] -> [T.Text] -> IO Response
executeProcessWithArgs command args asMenu preamble postamble = do
    result <- try (P.readProcess command args "") :: IO (Either SomeException String)
    let contents =
          case result of
            Left err  ->
                if asMenu
                    then [error' . T.pack $ "Process execution failed: " ++ show err]
                    else [T.pack $ "Process execution failed: " ++ show err]
            Right out ->
                if asMenu
                    then
                        info . T.pack <$> lines out
                    else
                        [T.pack out]
    return . TextResponse . render $ preamble ++ contents ++ postamble