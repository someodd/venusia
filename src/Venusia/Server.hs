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
  -- * Server configuration
  , ServerConfig(..)
  , defaultServerConfig
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

-- | Default bytes pulled from a client in the single initial @recv@.
--
-- This is a /single/ @recv@, not an accumulating read loop, so it is the
-- ceiling on how much of a request the server ever looks at: anything the
-- client sends beyond this sits unread in the kernel buffer and is discarded
-- when the connection closes. It is therefore the per-connection request read
-- buffer, /not/ a guaranteed maximum request size — a request split across TCP
-- segments could arrive partial (irrelevant in practice; a Gopher selector,
-- optional TAB, and search string arrive in one segment).
--
-- 4 KB leaves comfortable headroom for long search queries while staying tiny
-- per connection. Overridable per deployment via the @[server]@ config table
-- (@request_buffer_bytes@); see 'ServerConfig'.
{-@ defaultRequestBufferBytes :: {v:Int | v > 0} @-}
defaultRequestBufferBytes :: Int
defaultRequestBufferBytes = 4096

-- | How long the server will wait for a client to send the request line before
-- giving up. Defends against slowloris-style holders that open a connection and
-- never speak. Only applies to the initial @recv@; once a response is being
-- produced the producer sets the pace.
{-@ defaultReadTimeoutMicros :: {v:Int | v > 0} @-}
defaultReadTimeoutMicros :: Int
defaultReadTimeoutMicros = 30 * 1000 * 1000

-- | Maximum number of in-flight connections accepted at once.
--
-- Once this many handlers are running, the accept loop blocks until one
-- finishes. This caps FD usage and per-connection memory under load (or
-- attack — without a cap a flood of connections can exhaust the server's
-- file-descriptor limit before each handler completes).
--
-- 256 leaves headroom against the typical 1024 default soft @ulimit -n@.
-- Operators serving more concurrent traffic should both raise that limit
-- and the @[server]@ @max_connections@ key (see 'ServerConfig').
{-@ defaultMaxConcurrentConnections :: {v:Int | v > 0} @-}
defaultMaxConcurrentConnections :: Int
defaultMaxConcurrentConnections = 256

-- | Linux @TCP_USER_TIMEOUT@: bound on how long the kernel waits for an ACK
-- before declaring the connection dead. This is the only line of defence
-- against a slow-reading client that holds a streaming response open
-- indefinitely (the read-side read timeout guard doesn't apply once
-- a response is being written).
--
-- Set to two minutes — generous enough that legitimate clients on flaky
-- mobile links survive, tight enough that an attacker's silently-stalled
-- connection is reaped instead of pinning a thread + FD forever.
--
-- No-op on platforms that don't support the option (BSD, macOS).
{-@ defaultConnectionWriteTimeoutMillis :: {v:Int | v > 0} @-}
defaultConnectionWriteTimeoutMillis :: Int
defaultConnectionWriteTimeoutMillis = 120 * 1000

-- | Operator-tunable server settings, parsed from the @[server]@ table of the
-- config file (see "Venusia.Routes") and threaded into the accept loop and
-- per-connection handler. A missing @[server]@ table yields
-- 'defaultServerConfig'.
--
-- These are read /once at startup/ and are not hot-reloaded: unlike route
-- definitions, changing @[server]@ requires a server restart (the concurrency
-- semaphore in 'runOnSocket' is created once and cannot meaningfully change
-- mid-run, so all four keys share that restart-required contract for
-- consistency).
data ServerConfig = ServerConfig
  { requestBufferBytes           :: Int
  -- ^ Bytes for the single initial @recv@ (see 'defaultRequestBufferBytes').
  , maxConcurrentConnections     :: Int
  -- ^ Accept-loop concurrency cap (see 'defaultMaxConcurrentConnections').
  , readTimeoutMicros            :: Int
  -- ^ Initial-read timeout in microseconds (see 'defaultReadTimeoutMicros').
  , connectionWriteTimeoutMillis :: Int
  -- ^ Write-side @TCP_USER_TIMEOUT@ in milliseconds
  -- (see 'defaultConnectionWriteTimeoutMillis').
  } deriving (Show, Eq)

-- | The built-in defaults, used when no @[server]@ table is present and as the
-- per-key fallback for any key the operator omits.
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { requestBufferBytes           = defaultRequestBufferBytes
  , maxConcurrentConnections     = defaultMaxConcurrentConnections
  , readTimeoutMicros            = defaultReadTimeoutMicros
  , connectionWriteTimeoutMillis = defaultConnectionWriteTimeoutMillis
  }

-- | Milliseconds 'gracefulClose' will spend draining unread inbound bytes and
-- waiting for the peer's FIN before closing the socket. A legitimate client
-- sends its FIN within a round-trip, so this is effectively a safety cap
-- against a client that reads the body but never closes its half.
--
-- Without the drain, @close()@ on a socket that still has unread data in its
-- receive buffer makes the kernel abort the connection with a TCP RST instead
-- of a clean FIN. That surfaces to clients as \"connection reset by peer\"
-- (curl error 56) and trashes their exit code even though the response body
-- was delivered in full.
{-@ gracefulCloseTimeoutMillis :: {v:Int | v > 0} @-}
gracefulCloseTimeoutMillis :: Int
gracefulCloseTimeoutMillis = 2000

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
    hasStar        = not (T.null rest)

    -- For mount-style patterns (no '*'), the prefix must align on a
    -- path-segment boundary, so a route at @/applets@ matches @/applets@
    -- and @/applets/foo@ but not @/applets.bcard@ or @/appletsville@.
    -- A previous version used a bare 'T.stripPrefix', which let
    -- @/applets.bcard@ slip into the @/applets@ mount and be resolved
    -- inside its served root as @.bcard@ — a dotfile, refused by the
    -- file-server's path guard with a misleading "dotfile" error.
    -- Wildcard patterns (containing '*') keep substring semantics: the
    -- author asked for them.
    segmentAligned afterPrefix =
      T.null afterPrefix
      || "/" `T.isSuffixOf` prefix
      || "/" `T.isPrefixOf` afterPrefix

    -- Try to match @sel@ against the pattern. Returns the part that the
    -- wildcard captured.
    tryMatch sel = case T.stripPrefix prefix sel of
      Just afterPrefix
        | T.isSuffixOf suffix afterPrefix
        , hasStar || segmentAligned afterPrefix ->
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
-- Concurrency is bounded by @cfg.maxConcurrentConnections@ via a semaphore:
-- when the cap is reached, the loop blocks on @accept@ until an in-flight
-- handler finishes. This caps FD usage under connection floods. Each
-- accepted socket also has @cfg.connectionWriteTimeoutMillis@ applied as a
-- write-side timeout (Linux @TCP_USER_TIMEOUT@; no-op elsewhere) so a
-- slow-reading client cannot pin a streaming response forever.
runOnSocket :: ServerConfig -> Socket -> Handler -> MVar [Route] -> IO ()
runOnSocket cfg sock noMatch routesMVar = do
  sem <- newQSem cfg.maxConcurrentConnections
  forever $ do
    waitQSem sem
    (conn, _) <- accept sock
    trySetSocketOption conn UserTimeout cfg.connectionWriteTimeoutMillis
    forkFinally
      (handleConnHotReload cfg noMatch routesMVar conn)
      (\_ -> signalQSem sem)

-- | Main server loop with hot-reloading support.
serveHotReload
  :: ServerConfig   -- ^ Operator-tunable server settings.
  -> String         -- ^ Port to listen on.
  -> Handler      -- ^ Handler for invalid selectors.
  -> MVar [Route]   -- ^ A mutable reference to the list of routes.
  -> IO ()
serveHotReload cfg port noMatch routesMVar = withSocketsDo $ do
  addr <- resolve port
  sock <- open addr
  putStrLn $ "Gopher server (hot-reload enabled) running on port " ++ port
  runOnSocket cfg sock noMatch routesMVar
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

-- | Close a client socket with a clean TCP teardown. 'gracefulClose' sends
-- FIN, drains any unread inbound bytes (so the kernel doesn't abort the
-- connection with an RST), then closes — closing the descriptor internally
-- even if the drain throws. We swallow teardown errors so a half-gone peer
-- can't turn connection cleanup into a logged handler error.
closeGracefully :: Socket -> IO ()
closeGracefully sock =
  gracefulClose sock gracefulCloseTimeoutMillis
    `catch` \(_ :: SomeException) -> close sock

-- | Handle a connection using a dynamic list of routes from an MVar.
--
-- The socket is always closed via 'finally' so a thrown handler (or a
-- streaming producer that hits a broken pipe) cannot leak the file
-- descriptor. Cleanup goes through 'closeGracefully', which drains any unread
-- request bytes and sends a FIN rather than an abortive RST. The initial
-- @recv@ reads up to @cfg.requestBufferBytes@ and is bounded by
-- @cfg.readTimeoutMicros@; if the client never sends a request we drop the
-- connection rather than holding a thread. Synchronous exceptions thrown by
-- the handler or producer are caught and logged via 'logHandlerException' so
-- they don't tear down the forked thread with an uncaught error dump.
handleConnHotReload :: ServerConfig -> Handler -> MVar [Route] -> Socket -> IO ()
handleConnHotReload cfg noMatch routesMVar sock =
  (body `catch` logHandlerException) `finally` closeGracefully sock
  where
    body = do
      clientIp <- peerIpText sock
      mreq <- timeout cfg.readTimeoutMicros (NBS.recv sock cfg.requestBufferBytes)
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
