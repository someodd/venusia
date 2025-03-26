{-| Internet Gopher Protocol server framework.
-}
module Venusia.Server where

import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.List (isPrefixOf)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

-- | Standard invalid selector response
invalidSelector :: T.Text
invalidSelector = error "Invalid selector"

-- | A Gopher request with selector and optional query
data Request = Request
  { reqSelector :: T.Text
  -- ^ The selector part (before tab)
  , reqWildcard :: Maybe T.Text
  -- ^ Captured wildcard content (if any)
  , reqQuery    :: Maybe T.Text
  -- ^ The query part (after tab, if any)
  } deriving (Show)

-- FIXME: should be m BS.ByteString!
-- | A handler processes a request and returns a response
type Handler = Request -> IO T.Text

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
dispatch :: [Route] -> T.Text -> IO T.Text
dispatch routes req = go routes
  where
    go [] = return invalidSelector
    go (Route match handle : rs) =
      case match req of
        Just request -> handle request
        Nothing -> go rs

-- | Start the Gopher server on the specified port
serve :: String -> [Route] -> IO ()
serve port routes = withSocketsDo $ do
  addr <- resolve port
  sock <- open addr
  putStrLn $ "Gopher server running on port " ++ port
  forever $ do
    (conn, _) <- accept sock
    void $ forkIO $ handleConn routes conn
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
handleConn :: [Route] -> Socket -> IO ()
handleConn routes sock = do
  req <- NBS.recv sock 1024
  let trimmedReq = BS8.strip req
  putStrLn $ "Received request: " ++ show trimmedReq
  response <- dispatch routes (TE.decodeUtf8 trimmedReq)
  NBS.sendAll sock (TE.encodeUtf8 response)
  close sock