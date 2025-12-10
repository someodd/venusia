{-| Internet Gopher Protocol server framework.
-}
module Venusia.Server (
  -- * Request and response types
    Request(..)
  , Response(..)
  , Handler
  , Route(..)
  -- * Route matching
  , on
  , onWildcard
  -- * Default handlers
  , noMatchHandler
  -- * Server setup and running
  , serveHotReload
) where

import Venusia.MenuBuilder
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent (forkIO, MVar, readMVar) -- CHANGED: Add MVar imports
import Control.Monad (forever, void, unless)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Prelude hiding (error)
import System.IO (withFile, IOMode (..))

-- | Size of chunks to read when streaming files.
--
-- I think 32 KB is a sweet spot for low memory footprint, good throughput, minimal
-- syscall overhead, avoiding blowing L1/L2 caches, matches typical kernel socket buffer
-- sizes.  It's (?) widely used across nginx, Apache, Haskell Warp, etc.
chunkSize :: Int
chunkSize = 32768

-- | A Gopher request with selector and optional query
data Request = Request
  { reqSelector :: T.Text
  -- ^ The selector part (before tab)
  , reqWildcard :: Maybe T.Text
  -- ^ Captured wildcard content (if any)
  , reqQuery    :: Maybe T.Text
  -- ^ The query part (after tab, if any)
  } deriving (Show)

-- | A response can either be text or binary data
data Response
  = TextResponse T.Text
  -- ^ In-memory text response.
  | BinaryResponse BS.ByteString
  -- ^ In-memory bytes.
  | FileResponse FilePath
  -- ^ Serve a file from disk. What you should probably use by default as not to have a
  -- giant reserved heap (so we can have a constant size memory)

-- | A handler processes a request and returns a response
type Handler = Request -> IO Response

-- | A route matches requests and handles them
data Route = Route
  { matchRoute :: T.Text -> Maybe Request
  , runHandler :: Handler
  }

-- | Parse a raw Gopher request into selector and query parts
parseRequest :: T.Text -> (T.Text, Maybe T.Text)
parseRequest raw = 
  case T.split (== '\t') raw of
    (sel:q:_) -> (sel, Just q)
    [sel]     -> (sel, Nothing)
    _         -> (raw, Nothing)

-- | Create a route for exact selector matching
on :: T.Text -> Handler -> Route
on path handler = Route matcher handler
  where
    matcher raw = 
      let (sel, q) = parseRequest raw
      in if sel == path
         then Just $ Request sel Nothing q
         else Nothing

-- | Create a route with wildcard path matching
onWildcard :: T.Text -> Handler -> Route
onWildcard pattern handler = Route matcher handler
  where
    matcher raw =
      let (sel, q) = parseRequest raw
          (prefix, rest) = T.breakOn "*" pattern
          suffix = T.drop 1 rest  -- Skip the "*" character
      in case T.stripPrefix prefix sel of
           Just afterPrefix ->
             -- Now check if the remaining part ends with suffix
             if T.isSuffixOf suffix afterPrefix
               then 
                 -- Calculate the part that matched the wildcard
                 let wildcardLen = T.length afterPrefix - T.length suffix
                     wildcardPart = T.take wildcardLen afterPrefix
                 in Just $ Request 
                      { reqSelector = sel
                      , reqWildcard = Just wildcardPart
                      , reqQuery = q
                      }
               else Nothing
           Nothing -> Nothing

-- | Dispatch a request to the first matching route
dispatch :: Handler -> [Route] -> T.Text -> IO Response
dispatch noMatch routes req = go routes
  where
    go [] =
      -- No matching route found, call the noMatch handler
      -- to present an error message (generally).
      let (sel, q) = parseRequest req
      in noMatch $ Request sel Nothing q
    go (Route match handle : rs) =
      case match req of
        Just request -> handle request
        Nothing -> go rs

{- | A simple default handler for the error case of
no matching route for the provided selector.

-}
noMatchHandler :: Handler
noMatchHandler request =
  return . TextResponse . render $
    [error' $ "Not found: " <> request.reqSelector]

-- | Convert a Response to ByteString for sending over the wire
responseToByteString :: Response -> BS.ByteString
responseToByteString (TextResponse text) = TE.encodeUtf8 text
responseToByteString (BinaryResponse bytes) = bytes


-- | Main server loop with hot-reloading support.
serveHotReload
  :: String         -- ^ Port to listen on.
  -> Handler      -- ^ Handler for invalid selectors.
  -> MVar [Route]   -- ^ A mutable reference to the list of routes.
  -> IO ()
serveHotReload port noMatch routesMVar = withSocketsDo $ do
  addr <- resolve port
  sock <- open addr
  putStrLn $ "Gopher server (hot-reload enabled) running on port " ++ port
  forever $ do
    (conn, _) <- accept sock
    -- Each connection now gets the MVar to read the latest routes.
    void $ forkIO $ handleConnHotReload noMatch routesMVar conn
  where
    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just p)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

-- | Handle a connection using a dynamic list of routes from an MVar.
handleConnHotReload :: Handler -> MVar [Route] -> Socket -> IO ()
handleConnHotReload noMatch routesMVar sock = do
  req <- NBS.recv sock 1024
  let trimmedReq = BS8.strip req
  -- Read the most current routes from the MVar for every request.
  currentRoutes <- readMVar routesMVar
  response <- dispatch noMatch currentRoutes (TE.decodeUtf8 trimmedReq)
  case response of
    FileResponse fp -> streamFile fp sock
    _               ->
      NBS.sendAll sock (responseToByteString response)
  close sock

-- | Stream a file over the socket in chunks. This helps the heap not balloon in size.
streamFile :: FilePath -> Socket -> IO ()
streamFile fp sock =
    withFile fp ReadMode $ \h -> let
        loop = do
            chunk <- BS.hGetSome h chunkSize
            unless (BS.null chunk) $ do
                NBS.sendAll sock chunk
                loop
        in loop