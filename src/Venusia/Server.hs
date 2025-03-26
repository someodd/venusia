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
  , serve
) where

import Venusia.MenuBuilder
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Prelude hiding (error)

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
  | BinaryResponse BS.ByteString

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

-- | Start the Gopher server on the specified port
serve
  :: String
  -- ^ Port to listen on.
  -> Handler
  -- ^ Handler for invalid selectors.
  -> [Route]
  -> IO ()
serve port noMatch routes = withSocketsDo $ do
  addr <- resolve port
  sock <- open addr
  putStrLn $ "Gopher server running on port " ++ port
  forever $ do
    (conn, _) <- accept sock
    void $ forkIO $ handleConn noMatch routes conn
  where
    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE]
                             , addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just p)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

-- | Handle an individual connection
handleConn :: Handler -> [Route] -> Socket -> IO ()
handleConn noMatch routes sock = do
  req <- NBS.recv sock 1024
  let trimmedReq = BS8.strip req
  putStrLn $ "Received request: " ++ show trimmedReq
  response <- dispatch noMatch routes (TE.decodeUtf8 trimmedReq)
  NBS.sendAll sock (responseToByteString response)
  close sock