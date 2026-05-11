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
  -- * Streaming helpers
  , streamFromHandle
  , chunkSize
  -- * Server setup and running
  , serveHotReload
  , runOnSocket
  -- * Internal (exported for testing)
  , parseRequest
  , sanitizeSelector
) where

import Venusia.MenuBuilder
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Control.Concurrent (forkFinally, MVar, readMVar)
import Control.Concurrent.QSem (newQSem, waitQSem, signalQSem)
import Control.Exception
  ( IOException
  , SomeAsyncException
  , SomeException
  , catch
  , displayException
  , finally
  , fromException
  , throwIO
  , try
  )
import Control.Monad (forever, unless)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Prelude hiding (error)
import System.IO (Handle, IOMode (..), hPutStrLn, stderr, withFile)
import System.Timeout (timeout)

-- | Size of chunks to read when streaming files.
--
-- I think 32 KB is a sweet spot for low memory footprint, good throughput, minimal
-- syscall overhead, avoiding blowing L1/L2 caches, matches typical kernel socket buffer
-- sizes.  It's (?) widely used across nginx, Apache, Haskell Warp, etc.
{-@ chunkSize :: {v:Int | v > 0} @-}
chunkSize :: Int
chunkSize = 32768

-- | How long the server will wait for a client to send the request line before
-- giving up. Defends against slowloris-style holders that open a connection and
-- never speak. Only applies to the initial @recv@; once a response is being
-- produced the producer sets the pace.
{-@ readTimeoutMicros :: {v:Int | v > 0} @-}
readTimeoutMicros :: Int
readTimeoutMicros = 30 * 1000 * 1000

-- | Maximum number of in-flight connections accepted at once.
--
-- Once this many handlers are running, the accept loop blocks until one
-- finishes. This caps FD usage and per-connection memory under load (or
-- attack — without a cap a flood of connections can exhaust the server's
-- file-descriptor limit before each handler completes).
--
-- 256 leaves headroom against the typical 1024 default soft @ulimit -n@.
-- Operators serving more concurrent traffic should both raise that limit
-- and edit this constant.
{-@ maxConcurrentConnections :: {v:Int | v > 0} @-}
maxConcurrentConnections :: Int
maxConcurrentConnections = 256

-- | Linux @TCP_USER_TIMEOUT@: bound on how long the kernel waits for an ACK
-- before declaring the connection dead. This is the only line of defence
-- against a slow-reading client that holds a streaming response open
-- indefinitely (the read-side 'readTimeoutMicros' guard doesn't apply once
-- a response is being written).
--
-- Set to two minutes — generous enough that legitimate clients on flaky
-- mobile links survive, tight enough that an attacker's silently-stalled
-- connection is reaped instead of pinning a thread + FD forever.
--
-- No-op on platforms that don't support the option (BSD, macOS).
{-@ connectionWriteTimeoutMillis :: {v:Int | v > 0} @-}
connectionWriteTimeoutMillis :: Int
connectionWriteTimeoutMillis = 120 * 1000

-- | A Gopher request with selector and optional query
data Request = Request
  { reqSelector :: T.Text
  -- ^ The selector part (before tab)
  , reqWildcard :: Maybe T.Text
  -- ^ Captured wildcard content (if any)
  , reqQuery    :: Maybe T.Text
  -- ^ The query part (after tab, if any)
  , reqClientIp :: T.Text
  -- ^ Peer's IP address as text (IPv4 dotted-quad or IPv6 colon
  -- form, depending on connection family); the empty string when
  -- the address can't be determined (e.g. a unix-socket peer or a
  -- 'getPeerName' failure). Populated by the accept loop before
  -- handler dispatch; not set by 'on' / 'onWildcard' themselves.
  } deriving (Show, Eq)

-- | A response that the server will write back to the client.
--
-- Pick the constructor that matches your payload's lifecycle:
--
-- * 'TextResponse' / 'BinaryResponse' — small, in-memory payloads (menus,
--   error pages, single short blobs). Simple but materialises everything in
--   memory before the first byte goes out.
-- * 'FileResponse' — a static file on disk. Streamed in 'chunkSize' chunks,
--   so memory stays constant regardless of file size.
-- * 'StreamingResponse' — anything else: generated content, a process pipe,
--   a TCP relay (e.g. an internet-radio stream proxied over Gopher), a live
--   tail. Memory stays constant; the producer chooses when each chunk goes
--   out.
data Response
  = TextResponse T.Text
  -- ^ In-memory text response.
  | BinaryResponse BS.ByteString
  -- ^ In-memory bytes.
  | FileResponse FilePath
  -- ^ Serve a file from disk. Streamed in fixed-size chunks; constant memory.
  | StreamingResponse ((BS.ByteString -> IO ()) -> IO ())
  -- ^ Constant-memory streaming response. The producer is given a @send@
  -- action and runs until it returns (clean end-of-stream) or throws (client
  -- gone, or producer error). The producer owns its own resources and should
  -- wrap acquisitions in 'Control.Exception.bracket' so cleanup runs whether
  -- the stream ends normally, the client disconnects, or an exception is
  -- raised. Example (relay an upstream HTTP audio stream):
  --
  -- > StreamingResponse $ \send ->
  -- >   bracket (connectUpstream url) hClose $ \h ->
  -- >     streamFromHandle h send

-- | A handler processes a request and returns a response
type Handler = Request -> IO Response

-- | A route matches requests and handles them
data Route = Route
  { matchRoute :: T.Text -> Maybe Request
  , runHandler :: Handler
  }

-- | Parse a raw Gopher request into selector and query parts.
--
-- The request line is split on the first horizontal tab:
--
-- * @parseRequest "sel"@                    @= ("sel", Nothing)@
-- * @parseRequest "sel\\tq"@                @= ("sel", Just "q")@
-- * @parseRequest "sel\\tq1\\tq2"@          @= ("sel", Just "q1")@ (extra tabs are dropped)
-- * @parseRequest ""@                       @= ("", Nothing)@
parseRequest :: T.Text -> (T.Text, Maybe T.Text)
parseRequest raw =
  case T.split (== '\t') raw of
    (sel:q:_) -> (sel, Just q)
    [sel]     -> (sel, Nothing)
    _         -> (raw, Nothing)

-- | Trim the request bytes to just the first line (per RFC 1436 the request
-- line ends at CRLF; anything after is not part of the selector). Strips
-- surrounding whitespace too.
--
-- Defends against embedded CR/LF being treated as part of the selector
-- (log-injection / response-smuggling) and clients that send bare @LF@
-- instead of @CRLF@. The "no CR/LF in output" invariant is enforced by a
-- QuickCheck property in @Test.Venusia.Server@; see the README for the
-- corresponding LiquidHaskell extension-point spec.
sanitizeSelector :: BS.ByteString -> BS.ByteString
sanitizeSelector = BS8.strip . BS8.takeWhile (\c -> c /= '\r' && c /= '\n')

-- | Create a route for exact selector matching
on :: T.Text -> Handler -> Route
on path handler = Route matcher handler
  where
    matcher raw =
      let (sel, q) = parseRequest raw
      in if sel == path
         then Just $ Request sel Nothing q T.empty
         else Nothing

-- | Create a route with wildcard path matching.
--
-- Trailing-slash equivalence: when the pattern is a "directory" selector
-- (ends with @\'/\'@ and contains no @\'*\'@), the version without the
-- trailing slash matches too. Common-sense usability — clients vary on
-- whether they normalise trailing slashes, and a request for
-- @/applets@ should reach the same handler as @/applets/@.
-- 'reqSelector' is preserved as the request actually arrived (so the
-- handler can render its own canonical link if it wants).
onWildcard :: T.Text -> Handler -> Route
onWildcard pattern handler = Route matcher handler
  where
    (prefix, rest) = T.breakOn "*" pattern
    suffix         = T.drop 1 rest  -- skip the '*' character if present
    isDirSelector  = T.null rest && "/" `T.isSuffixOf` pattern

    -- Try to match @sel@ against the pattern. Returns the part that the
    -- wildcard captured.
    tryMatch sel = case T.stripPrefix prefix sel of
      Just afterPrefix
        | T.isSuffixOf suffix afterPrefix ->
            let wildcardLen  = T.length afterPrefix - T.length suffix
                wildcardPart = T.take wildcardLen afterPrefix
            in Just wildcardPart
        | otherwise -> Nothing
      Nothing -> Nothing

    matcher raw =
      let (sel, q) = parseRequest raw
          -- For dir-style patterns ("/applets/"), accept "/applets" too
          -- by mapping it to "/applets/" before matching.
          adjusted
            | isDirSelector && sel == T.dropEnd 1 pattern = pattern
            | otherwise                                   = sel
      in case tryMatch adjusted of
           Just wildcardPart -> Just $ Request
             { reqSelector = sel             -- as-received
             , reqWildcard = Just wildcardPart
             , reqQuery    = q
             , reqClientIp = T.empty         -- filled in by dispatch
             }
           Nothing -> Nothing

-- | Dispatch a request to the first matching route, stamping the
-- supplied client IP onto the resulting 'Request' before the handler
-- runs. Route matchers ('on' / 'onWildcard') construct 'Request'
-- values with @reqClientIp = ""@ since they have no access to the
-- connection; dispatch is the choke point that knows the peer.
dispatch :: Handler -> [Route] -> T.Text -> T.Text -> IO Response
dispatch noMatch routes clientIp req = go routes
  where
    setIp r = r { reqClientIp = clientIp }
    go [] =
      -- No matching route found, call the noMatch handler
      -- to present an error message (generally).
      let (sel, q) = parseRequest req
      in noMatch (setIp (Request sel Nothing q T.empty))
    go (Route match handle : rs) =
      case match req of
        Just request -> handle (setIp request)
        Nothing      -> go rs

{- | A simple default handler for the error case of
no matching route for the provided selector.

-}
noMatchHandler :: Handler
noMatchHandler request =
  return . TextResponse . render $
    [error' $ "Not found: " <> request.reqSelector]

-- | Stream from an open 'Handle' in 'chunkSize'-byte pieces, calling @send@
-- on each non-empty chunk. Returns when the handle hits EOF.
streamFromHandle :: Handle -> (BS.ByteString -> IO ()) -> IO ()
streamFromHandle h send = loop
  where
    loop = do
      chunk <- BS.hGetSome h chunkSize
      unless (BS.null chunk) $ do
        send chunk
        loop

-- | Write a 'Response' to the connected socket using the appropriate strategy.
sendResponse :: Socket -> Response -> IO ()
sendResponse sock response = case response of
  TextResponse t      -> NBS.sendAll sock (TE.encodeUtf8 t)
  BinaryResponse b    -> NBS.sendAll sock b
  FileResponse fp     -> withFile fp ReadMode $ \h ->
                           streamFromHandle h (NBS.sendAll sock)
  StreamingResponse f -> f (NBS.sendAll sock)

-- | Set a socket option, silently ignoring \"not supported\" errors so that
-- platform-specific options (like @TCP_USER_TIMEOUT@ on Linux) don't crash
-- the server when the kernel doesn't recognise them.
trySetSocketOption :: Socket -> SocketOption -> Int -> IO ()
trySetSocketOption sock opt val =
  setSocketOption sock opt val `catch` (\(_ :: IOException) -> pure ())

-- | Run the accept loop on an already-bound, listening socket.
--
-- Useful when you need to control socket setup yourself — for example,
-- binding to an ephemeral port for tests, dropping privileges before
-- accepting, or wiring TLS in front. 'serveHotReload' is the convenient
-- wrapper that does the bind/listen for you.
--
-- Concurrency is bounded by 'maxConcurrentConnections' via a semaphore: when
-- the cap is reached, the loop blocks on @accept@ until an in-flight
-- handler finishes. This caps FD usage under connection floods. Each
-- accepted socket also has 'connectionWriteTimeoutMillis' applied as a
-- write-side timeout (Linux @TCP_USER_TIMEOUT@; no-op elsewhere) so a
-- slow-reading client cannot pin a streaming response forever.
runOnSocket :: Socket -> Handler -> MVar [Route] -> IO ()
runOnSocket sock noMatch routesMVar = do
  sem <- newQSem maxConcurrentConnections
  forever $ do
    waitQSem sem
    (conn, _) <- accept sock
    trySetSocketOption conn UserTimeout connectionWriteTimeoutMillis
    forkFinally
      (handleConnHotReload noMatch routesMVar conn)
      (\_ -> signalQSem sem)

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
  runOnSocket sock noMatch routesMVar
  where
    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) Nothing (Just p)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      setSocketOption sock NoDelay 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

-- | Log a synchronous handler exception to stderr and swallow it. Async
-- exceptions ('SomeAsyncException', i.e. 'killThread' / 'throwTo' from the
-- supervisor) are re-thrown so cooperative shutdown still works.
logHandlerException :: SomeException -> IO ()
logHandlerException e = case fromException e :: Maybe SomeAsyncException of
  Just _  -> throwIO e
  Nothing -> hPutStrLn stderr $
    "venusia: connection handler error: " ++ displayException e

-- | Handle a connection using a dynamic list of routes from an MVar.
--
-- The socket is always closed via 'finally' so a thrown handler (or a
-- streaming producer that hits a broken pipe) cannot leak the file
-- descriptor. The initial @recv@ is bounded by 'readTimeoutMicros'; if the
-- client never sends a request we drop the connection rather than holding a
-- thread. Synchronous exceptions thrown by the handler or producer are
-- caught and logged via 'logHandlerException' so they don't tear down the
-- forked thread with an uncaught error dump.
handleConnHotReload :: Handler -> MVar [Route] -> Socket -> IO ()
handleConnHotReload noMatch routesMVar sock =
  (body `catch` logHandlerException) `finally` close sock
  where
    body = do
      clientIp <- peerIpText sock
      mreq <- timeout readTimeoutMicros (NBS.recv sock 1024)
      case mreq of
        Nothing  -> return ()  -- slowloris / silent client
        Just raw -> do
          let trimmedReq = sanitizeSelector raw
          currentRoutes <- readMVar routesMVar
          response <- dispatch noMatch currentRoutes clientIp (TE.decodeUtf8 trimmedReq)
          sendResponse sock response

-- | Return the peer's IP as text (IPv4 dotted-quad or IPv6 colon
-- form), or the empty string when the peer can't be looked up.
-- Wraps 'getPeerName' + 'getNameInfo' with 'NI_NUMERICHOST' so the
-- result is always the literal address, never a DNS reverse-lookup.
-- All exceptions are swallowed and surfaced as @""@ — the goal is
-- "best-effort metadata for scripts", not a hard guarantee.
peerIpText :: Socket -> IO T.Text
peerIpText sock = do
  result <- try $ do
    sa <- getPeerName sock
    (mhost, _) <- getNameInfo [NI_NUMERICHOST] True False sa
    pure (maybe T.empty T.pack mhost)
  case (result :: Either SomeException T.Text) of
    Left _  -> pure T.empty
    Right t -> pure t
