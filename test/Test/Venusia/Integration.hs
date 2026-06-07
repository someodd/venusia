-- | End-to-end tests that bind a real socket and speak Gopher to it. These
-- exercise the full request → dispatch → 'sendResponse' → close pipeline,
-- including the four 'Response' constructors, RFC sanitisation, and FD safety
-- under broken-pipe conditions.
module Test.Venusia.Integration (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString as BS
import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket, catch, try, IOException, SomeException)
import Control.Monad (forM_, replicateM_)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)

import Venusia.FileHandler (fileExtensionToItemType, serveDirectory, serveDirectoryWith)
import Venusia.Server
  ( Route(..)
  , Request(..)
  , Response(..)
  , noMatchHandler
  , on
  , onWildcard
  , runOnSocket
  , defaultServerConfig
  )
import Venusia.Routes
  ( ScriptExtensionConfig(..)
  , FileTypeConfig(..)
  , FilesConfig(..)
  , RoutesConfig(..)
  , mkScriptHook
  , resolveItemType
  , routesConfigCodec
  , runProcess
  , splitForPathInfo
  )
import qualified Toml

tests :: TestTree
tests = testGroup "integration"
  [ testCase "TextResponse is delivered verbatim"           test_textResponse
  , testCase "BinaryResponse is delivered verbatim"         test_binaryResponse
  , testCase "FileResponse streams a file end to end"       test_fileResponse
  , testCase "StreamingResponse delivers chunks in order"   test_streamingResponse
  , testCase "StreamingResponse over a large generated body is correct"
                                                             test_streamingLargeBody
  , testCase "Embedded CRLF in request is truncated to first line"
                                                             test_embeddedCRLF
  , testCase "Empty selector falls through to noMatchHandler"
                                                             test_emptySelector
  , testCase "Tab-separated query is parsed and delivered to handler"
                                                             test_tabQuery
  , testCase "Connection is closed after each response (server-initiated EOF)"
                                                             test_serverClosesAfterResponse
  , testCase "Connection closes cleanly (FIN, not RST) when client left unread bytes"
                                                             test_cleanCloseWithUnreadInput
  , testCase "FD does not leak when the producer throws partway through"
                                                             test_producerThrowsNoLeak
  , testCase "Many sequential requests do not leak state"   test_manySequentialRequests
  , testCase "runProcess streams a process's stdout end to end"
                                                             test_runProcessStreaming
  , testCase "runProcess wraps each line as info-line item when as_info_lines is true"
                                                             test_runProcessInfoWrap
  , testCase "runProcess (buffered, raw) preserves legacy menu=false behavior"
                                                             test_runProcessBufferedRaw
  , testCase "script_extension hook runs the registered script"
                                                             test_scriptHookExecutesFile
  , testCase "script_extension hook ignores files of other extensions"
                                                             test_scriptHookFallsThrough
  , testCase "streaming script_extension delivers chunked stdout"
                                                             test_scriptStreamingExecution
  , testCase "resolveItemType: file_type wins over script_extension wins over default"
                                                             test_resolveItemType
  , testCase "streaming info-wrap strips trailing CR from CRLF output"
                                                             test_streamingInfoWrapStripsCR
  , testCase "extension lookup is case-insensitive on both sides"
                                                             test_resolveItemTypeCaseInsensitive
  , testCase "directory traversal is blocked when sibling shares a string prefix"
                                                             test_directoryTraversalGuard
  , testCase "listing at the served root produces leading-slash selectors, no ./ artifact"
                                                             test_listingSelectorAtServedRoot
  , testCase "listing at a non-empty block's root links to the gopher-level parent"
                                                             test_listingParentLinkAtBlockRoot
  , testCase "listing at the catch-all root has no parent link"
                                                             test_listingNoParentAtCatchallRoot
  , testCase "listing hides dotfiles by default (.giosaveXXX, .git, .gophermap)"
                                                             test_listingHidesDotfiles
  , testCase "direct request for a dotfile path is refused when allowDotfiles=False"
                                                             test_directDotfileRequestRefused
  , testCase "direct request for the configured indexFile (e.g. .gophermap) is allowed"
                                                             test_directIndexFileRequestAllowed
  , testCase "request traversing a non-final dotfile component (.git/config) is refused"
                                                             test_intermediateDotfileRefused
  , testCase "dotfiles are listed AND served when allowDotfiles=True"
                                                             test_listingShowsDotfilesWhenOptedIn
  , testCase "unlisted glob patterns hide files from listing but allow direct fetch"
                                                             test_unlistedFiltersListingNotDirect
  , testCase "preamble/postamble lines auto-terminated with CRLF in info-wrap mode"
                                                             test_preamblePostambleCRLF
  , testCase "streaming child process is killed when client disconnects mid-stream"
                                                             test_streamingDisconnectKillsChild
  , testCase "script_extension $search is substituted from the request query"
                                                             test_scriptExtensionSearchSubstitution
  , testCase "script_extension $selector is substituted from the request selector"
                                                             test_scriptExtensionSelectorSubstitution
  , testCase "script_extension $remote_ip is substituted from the peer address"
                                                             test_scriptExtensionRemoteIpSubstitution
  , testCase "script_extension unrecognized tokens pass through verbatim"
                                                             test_scriptExtensionTokenPassthrough
  , testCase "$pathinfo carries the path-info suffix to the script"
                                                             test_scriptExtensionPathInfo
  , testCase "$pathinfo is empty when the request addresses the script directly"
                                                             test_scriptExtensionPathInfoEmpty
  , testCase "$pathinfo is \"/\" when the request has a bare trailing slash"
                                                             test_scriptExtensionPathInfoTrailingSlash
  , testCase "wildcard with no real script-prefix file falls through (no spurious exec)"
                                                             test_scriptExtensionPathInfoNoFile
  , testCase "$pathinfo and $search coexist when both are present"
                                                             test_scriptExtensionPathInfoSearchCoexists
  , testCase "path-info wildcard with .. traversal is rejected, not split"
                                                             test_scriptExtensionPathInfoTraversalGuard
  , testCase "tomland decodes [[files.script_extension]] + [[files.file_type]] nested arrays"
                                                             test_tomlandNestedArrayDecode
  ]

------------------------------------------------------------------------
-- withServer: bind ephemeral, run accept loop, hand the port to the test
------------------------------------------------------------------------

withServer :: [Route] -> (Int -> IO a) -> IO a
withServer routes action =
  bracket acquire release run
  where
    acquire :: IO (Socket, ThreadId, Int)
    acquire = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      setSocketOption sock NoDelay 1
      -- Loopback only: don't expose the test server on any other interface.
      bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen sock 16
      portRaw <- socketPort sock
      let p = fromIntegral portRaw :: Int
      routesVar <- newMVar routes
      tid <- forkIO $ runOnSocket defaultServerConfig sock noMatchHandler routesVar
      -- A tiny pause so accept() is established before the test connects.
      threadDelay 50_000
      pure (sock, tid, p)
    release (sock, tid, _) = do
      killThread tid
      close sock
    run (_, _, p) = action p

-- | Open a TCP connection to the local server, send the request, read until EOF.
gopherRoundtrip :: Int -> BS.ByteString -> IO BS.ByteString
gopherRoundtrip port req =
  bracket acquire close $ \sock -> do
    NBS.sendAll sock req
    drain sock mempty
  where
    acquire = do
      addr:_ <- getAddrInfo
        (Just defaultHints { addrSocketType = Stream })
        (Just "127.0.0.1") (Just (show port))
      s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect s (addrAddress addr)
      pure s
    drain sock acc = do
      chunk <- NBS.recv sock 4096
      if BS.null chunk
        then pure acc
        else drain sock (acc <> chunk)

------------------------------------------------------------------------
-- Tests for each Response constructor
------------------------------------------------------------------------

test_textResponse :: Assertion
test_textResponse = do
  let routes =
        [ on "/hi" $ \_ -> pure (TextResponse "hello world\r\n") ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/hi\r\n"
    resp @?= "hello world\r\n"

test_binaryResponse :: Assertion
test_binaryResponse = do
  let payload = BS.pack [0, 1, 2, 3, 0xFF, 0xFE, 0x42, 0x00, 0x80]
  let routes = [ on "/bin" $ \_ -> pure (BinaryResponse payload) ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/bin\r\n"
    resp @?= payload

test_fileResponse :: Assertion
test_fileResponse =
  withSystemTempFile "venusia-test-.bin" $ \fp h -> do
    -- Write ~256 KB so we know we cross the chunkSize boundary multiple times.
    let chunk = BS.replicate 1024 0x41
    forM_ [1 :: Int .. 256] $ \_ -> BS.hPut h chunk
    hClose h
    let expected = BS.concat (replicate 256 chunk)
    let routes = [ on "/file" $ \_ -> pure (FileResponse fp) ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/file\r\n"
      assertEqual "byte length matches" (BS.length expected) (BS.length resp)
      assertBool "byte content matches" (resp == expected)

test_streamingResponse :: Assertion
test_streamingResponse = do
  let routes =
        [ on "/s" $ \_ -> pure $ StreamingResponse $ \send -> do
            send "AAA"
            send "BBB"
            send "CCC"
        ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/s\r\n"
    resp @?= "AAABBBCCC"

test_streamingLargeBody :: Assertion
test_streamingLargeBody = do
  -- Emit 8 MB total in 8 KB chunks (1024 chunks). Verifies (a) ordering,
  -- (b) the read side really consumes everything, (c) no chunk truncation
  -- across kernel buffer boundaries.
  let oneChunk = BS.replicate 8192 0x5A  -- 'Z'
      total = 1024
  let routes =
        [ on "/big" $ \_ -> pure $ StreamingResponse $ \send ->
            replicateM_ total (send oneChunk)
        ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/big\r\n"
    BS.length resp @?= total * BS.length oneChunk
    assertBool "every byte is 'Z'" (BS.all (== 0x5A) resp)

------------------------------------------------------------------------
-- RFC / dispatch behaviour
------------------------------------------------------------------------

test_embeddedCRLF :: Assertion
test_embeddedCRLF = do
  -- A client that smuggles a second line should be treated as having sent
  -- only "/foo" — the second line is ignored.
  let routes = [ on "/foo" $ \req -> pure (TextResponse req.reqSelector) ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/foo\r\n/bar\r\n"
    resp @?= "/foo"

test_emptySelector :: Assertion
test_emptySelector = do
  withServer [] $ \port -> do
    resp <- gopherRoundtrip port "\r\n"
    BS.isInfixOf "Not found:" resp @?= True

test_tabQuery :: Assertion
test_tabQuery = do
  -- A type-7 search request: selector \t query \r\n.
  let routes =
        [ on "/search" $ \req ->
            pure (TextResponse (maybe "(no query)" id req.reqQuery))
        ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/search\thello world\r\n"
    resp @?= "hello world"

test_serverClosesAfterResponse :: Assertion
test_serverClosesAfterResponse = do
  -- gopherRoundtrip relies on the server closing the connection so `recv`
  -- returns empty. If the server held the socket open this test would hang
  -- (and tasty's per-test timeout would catch it). Reaching here is the
  -- assertion.
  let routes = [ on "/x" $ \_ -> pure (TextResponse "ok") ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/x\r\n"
    resp @?= "ok"

-- | A clean TCP teardown (FIN), not an abortive one (RST), even when the
-- client has sent bytes the server never reads. The server does a single
-- recv of cfg.requestBufferBytes (default 4096); the 8 KB of 'A's after the
-- selector line exceed that, so they sit unread in the kernel buffer when the
-- handler closes. A bare close()
-- on a socket with unread inbound data makes Linux emit RST instead of FIN —
-- which surfaces to the client as ECONNRESET (curl error 56) and trashes the
-- exit code even though the body arrived. 'gracefulClose' drains the unread
-- bytes first, so the read completes with a clean EOF.
test_cleanCloseWithUnreadInput :: Assertion
test_cleanCloseWithUnreadInput = do
  let routes = [ on "/hi" $ \_ -> pure (TextResponse "hello\r\n") ]
  withServer routes $ \port -> do
    let req = "/hi\r\n" <> BS.replicate 8192 0x41
    result <- try (gopherRoundtrip port req)
                :: IO (Either IOException BS.ByteString)
    case result of
      Left e  -> assertFailure ("connection reset instead of clean close: " <> show e)
      Right r -> assertBool "body delivered" (BS.isInfixOf "hello" r)

------------------------------------------------------------------------
-- FD safety / leak checks
------------------------------------------------------------------------

test_producerThrowsNoLeak :: Assertion
test_producerThrowsNoLeak = do
  -- Producer sends a few bytes then errors. The server's catch swallows the
  -- exception and finally closes the socket. We hammer the route a few
  -- times to confirm subsequent requests still go through (i.e. FDs aren't
  -- leaking).
  ref <- newIORef (0 :: Int)
  let routes =
        [ on "/boom" $ \_ -> pure $ StreamingResponse $ \send -> do
            send "partial"
            modifyIORef' ref (+1)
            error "boom"
        ]
  withServer routes $ \port -> do
    replicateM_ 10 $ do
      _ <- try (gopherRoundtrip port "/boom\r\n") :: IO (Either SomeException BS.ByteString)
      pure ()
    n <- readIORef ref
    assertBool "producer ran for every request" (n == 10)

test_manySequentialRequests :: Assertion
test_manySequentialRequests = do
  let routes = [ on "/p" $ \_ -> pure (TextResponse "pong") ]
  withServer routes $ \port -> do
    forM_ [1 :: Int .. 200] $ \_ -> do
      resp <- gopherRoundtrip port "/p\r\n"
      resp @?= "pong"

------------------------------------------------------------------------
-- runProcess: gateway/script-extension dispatch
------------------------------------------------------------------------

-- | stream=True, asInfoLines=False: process stdout piped through verbatim,
-- no terminator. Uses /bin/sh -c with a multi-line printf so the output
-- crosses send() boundaries.
test_runProcessStreaming :: Assertion
test_runProcessStreaming = do
  let handler _ = runProcess True False "/bin/sh"
                    ["-c", "printf 'aaa\\nbbb\\nccc\\n'"]
                    Nothing [] []
      routes = [ on "/stream" handler ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/stream\r\n"
    resp @?= "aaa\nbbb\nccc\n"

-- | stream=False, asInfoLines=True: legacy menu=true behaviour. Each stdout
-- line becomes an info-line ('iLINE\\t\\t\\t0\\r\\n') and the gopher
-- terminator is appended.
test_runProcessInfoWrap :: Assertion
test_runProcessInfoWrap = do
  let handler _ = runProcess False True "/bin/sh"
                    ["-c", "printf 'one\\ntwo\\n'"]
                    Nothing [] []
      routes = [ on "/menu" handler ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/menu\r\n"
    resp @?= "ione\t\t\t0\r\nitwo\t\t\t0\r\n.\r\n"

-- | stream=False, asInfoLines=False: legacy menu=false behaviour. Stdout
-- buffered as a single text blob, then terminator. Verifies back-compat
-- with the pre-existing executeProcessWithArgs path.
test_runProcessBufferedRaw :: Assertion
test_runProcessBufferedRaw = do
  let handler _ = runProcess False False "/bin/sh"
                    ["-c", "printf 'hello world'"]
                    Nothing [] []
      routes = [ on "/raw" handler ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/raw\r\n"
    resp @?= "hello world.\r\n"

------------------------------------------------------------------------
-- script_extension: file-server hook integration
------------------------------------------------------------------------

-- | Place a .sh file under a temp [[files]] root with a registered
-- script_extension; expect the hook to execute it instead of serving the
-- source.
test_scriptHookExecutesFile :: Assertion
test_scriptHookExecutesFile =
  withSystemTempDirectory "venusia-script-test" $ \dir -> do
    let scriptPath = dir </> "hello.sh"
    writeFile scriptPath "echo from-script\n"
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/sh"
              , arguments   = ["$file"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        hook       = mkScriptHook scriptExts "/files/marker.sh" Nothing "" ""
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp Nothing
                    hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/hello.sh\r\n"
      assertBool "script stdout present" (BS.isInfixOf "from-script" resp)

-- | A .txt file alongside a registered .sh script must NOT be intercepted.
-- The hook returns Nothing for unregistered extensions and the file is
-- served verbatim as a 'FileResponse'.
test_scriptHookFallsThrough :: Assertion
test_scriptHookFallsThrough =
  withSystemTempDirectory "venusia-script-test" $ \dir -> do
    let txtPath = dir </> "plain.txt"
    writeFile txtPath "this is the source\n"
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"     -- only .sh registered
              , command     = "/bin/sh"
              , arguments   = ["$file"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        hook       = mkScriptHook scriptExts "/files/marker.sh" Nothing "" ""
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp Nothing
                    hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/plain.txt\r\n"
      resp @?= "this is the source\n"

-- | A streaming script_extension uses 'StreamingResponse' under the hood.
-- Verifies the streaming code path reaches the wire when triggered through
-- the file-server hook (rather than directly via runProcess).
test_scriptStreamingExecution :: Assertion
test_scriptStreamingExecution =
  withSystemTempDirectory "venusia-script-test" $ \dir -> do
    let scriptPath = dir </> "drip.sh"
    writeFile scriptPath "printf 'one\\ntwo\\nthree\\n'\n"
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/sh"
              , arguments   = ["$file"]
              , stream      = Just True
              , asInfoLines = Nothing
              }
          ]
        hook       = mkScriptHook scriptExts "/files/marker.sh" Nothing "" ""
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp Nothing
                    hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/drip.sh\r\n"
      resp @?= "one\ntwo\nthree\n"

-- | Pure-logic test of the three-step item-type resolver. No sockets, no
-- temp dirs — just the function under test against a constructed input.
test_resolveItemType :: Assertion
test_resolveItemType = do
  let scriptExts =
        [ ScriptExtensionConfig
            { extension   = "lhs"
            , command     = "runghc"
            , arguments   = ["$file"]
            , stream      = Nothing
            , asInfoLines = Nothing      -- defaults item type to '0'
            }
        , ScriptExtensionConfig
            { extension   = "menu"
            , command     = "/bin/sh"
            , arguments   = ["$file"]
            , stream      = Nothing
            , asInfoLines = Just True    -- defaults item type to '1'
            }
        ]
      fileTypes =
        [ FileTypeConfig { extension = "lhs", itemType = "9" }  -- explicit override
        , FileTypeConfig { extension = "csv", itemType = "0" }  -- non-script override
        ]
      r = resolveItemType fileTypes scriptExts
  -- file_type override wins, even when a script_extension default exists.
  r "digest.lhs" @?= '9'
  -- script_extension default with as_info_lines=True maps to '1'.
  r "status.menu" @?= '1'
  -- file_type override applies to non-script extensions too.
  r "data.csv" @?= '0'
  -- Falls through to the hardcoded fileExtensionToItemType for known ones.
  r "image.png" @?= 'I'
  -- Falls through to the hardcoded default ('9' for unknown extensions).
  r "blob.xyz"  @?= '9'

-- | If the script outputs CRLF line endings, the streaming info-wrap path
-- must strip the trailing CR before wrapping. Otherwise the embedded CR
-- corrupts the gophermap line ('iLINE\\r\\t\\t\\t0\\r\\n').
test_streamingInfoWrapStripsCR :: Assertion
test_streamingInfoWrapStripsCR = do
  let handler _ = runProcess True True "/bin/sh"
                    ["-c", "printf 'one\\r\\ntwo\\r\\n'"]
                    Nothing [] []
      routes = [ on "/crlf" handler ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/crlf\r\n"
    resp @?= "ione\t\t\t0\r\nitwo\t\t\t0\r\n.\r\n"

-- | Extension matching is case-insensitive: a file with '.LHS' (uppercase)
-- matches a registered @extension = "lhs"@ (lowercase), and vice versa.
test_resolveItemTypeCaseInsensitive :: Assertion
test_resolveItemTypeCaseInsensitive = do
  let scriptExts =
        [ ScriptExtensionConfig
            { extension   = "lhs"
            , command     = "runghc"
            , arguments   = ["$file"]
            , stream      = Nothing
            , asInfoLines = Nothing
            }
        ]
      fileTypes =
        [ FileTypeConfig { extension = "MD", itemType = "0" } ]
      r = resolveItemType fileTypes scriptExts
  -- Uppercase file extension matches lowercase-registered script_extension.
  r "report.LHS"  @?= '0'
  -- Mixed-case file extension also matches.
  r "report.lHs"  @?= '0'
  -- Lowercase file extension matches uppercase-registered file_type.
  r "notes.md"    @?= '0'

------------------------------------------------------------------------
-- Directory-traversal guard (path-component check, not string-prefix)
------------------------------------------------------------------------

-- | A 'goodDir' served via [[files]] must NOT leak files from a sibling
-- directory whose name starts with the same string. Pre-fix, the guard
-- used 'isPrefixOf' on raw paths, so '/.../good' was treated as an
-- ancestor of '/.../good_evil/secret.txt' and the canonicalised
-- traversal slipped through.
test_directoryTraversalGuard :: Assertion
test_directoryTraversalGuard =
  withSystemTempDirectory "venusia-traversal-test" $ \parentDir -> do
    let goodDir = parentDir </> "good"
        evilDir = parentDir </> "good_evil"
    createDirectoryIfMissing False goodDir
    createDirectoryIfMissing False evilDir
    writeFile (goodDir </> "ok.txt") "OK\n"
    writeFile (evilDir </> "secret.txt") "leaked\n"
    let routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 goodDir "/files/" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      -- Sanity: legitimate file under the served root is still reachable.
      ok <- gopherRoundtrip port "/files/ok.txt\r\n"
      ok @?= "OK\n"
      -- The traversal must be denied, not silently leak the sibling.
      bad <- gopherRoundtrip port "/files/../good_evil/secret.txt\r\n"
      assertBool "traversal denied" (BS.isInfixOf "Access denied" bad)
      assertBool "secret content not present in response"
                 (not (BS.isInfixOf "leaked" bad))

-- | At the served root, 'makeRelative' returns @\".\"@ for the
-- requested path, which the older selector-building code passed
-- straight to '(</>)', producing wires like @\".\/catalog\"@ in
-- gophermap rows. Together with the empty @selector = \"\"@
-- catch-all, that produced bare names without a leading slash too.
-- Both must round-trip as clean absolute selectors.
test_listingSelectorAtServedRoot :: Assertion
test_listingSelectorAtServedRoot =
  withSystemTempDirectory "venusia-listing-root" $ \dir -> do
    writeFile (dir </> "catalog") "x\n"
    let routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/\r\n"
      assertBool "clean /catalog selector in listing"
                 (BS.isInfixOf "\t/catalog\t" resp)
      assertBool "no ./catalog artifact"
                 (not (BS.isInfixOf "/./catalog" resp))
      assertBool "no bare 'catalog' selector (must have leading /)"
                 (not (BS.isInfixOf "\tcatalog\t" resp))

-- | At the root of a non-empty-selector block, the listing must
-- offer a "Parent directory (..)" link pointing up in the gopher
-- namespace — without it, a user who navigates into /applets has no
-- way back to / via the auto-generated UI. Older builds suppressed
-- the parent row at every block root, leaving dead ends.
test_listingParentLinkAtBlockRoot :: Assertion
test_listingParentLinkAtBlockRoot =
  withSystemTempDirectory "venusia-listing-parent" $ \dir -> do
    writeFile (dir </> "x.txt") "hi\n"
    let routes =
          [ onWildcard "/applets" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "/applets" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/applets\r\n"
      assertBool "block-root listing includes a parent-directory row"
                 (BS.isInfixOf "Parent directory" resp)
      assertBool "parent points one level up in the gopher namespace (\"/\")"
                 (BS.isInfixOf "\t/\t" resp)

-- | At the catch-all root ("" selector) there isn't anywhere to go
-- up to — Venusia is the whole namespace from here — so the parent
-- row is correctly omitted. Guards against the previous fix being
-- over-broad.
test_listingNoParentAtCatchallRoot :: Assertion
test_listingNoParentAtCatchallRoot =
  withSystemTempDirectory "venusia-listing-noparent" $ \dir -> do
    writeFile (dir </> "x.txt") "hi\n"
    let routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/\r\n"
      assertBool "catch-all root has no parent-directory row"
                 (not (BS.isInfixOf "Parent directory" resp))

-- | Auto-generated listings hide Unix-style dotfiles. Otherwise
-- gvfs/SFTP temp turds (.giosaveXXXXX), VCS metadata (.git*), and
-- Venusia's own .gophermap leak into menus that clients render.
-- Files remain reachable by direct selector — they're just not
-- enumerated in the listing.
test_listingHidesDotfiles :: Assertion
test_listingHidesDotfiles =
  withSystemTempDirectory "venusia-listing-dotfiles" $ \dir -> do
    writeFile (dir </> "real.txt")        "visible\n"
    writeFile (dir </> ".giosave1234")    "internal\n"
    writeFile (dir </> ".gophermap-old")  "internal\n"
    let routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/\r\n"
      assertBool "visible file is enumerated"
                 (BS.isInfixOf "real.txt" resp)
      assertBool ".giosave1234 is hidden"
                 (not (BS.isInfixOf ".giosave1234" resp))
      assertBool ".gophermap-old is hidden"
                 (not (BS.isInfixOf ".gophermap-old" resp))

-- | Defence-in-depth: an attacker who guesses /foo/.env must not be
-- served the file even though it's not in any listing. The refusal
-- happens before file resolution, so the dotfile's content can't leak
-- via the response body or via timing differences against a non-
-- existent path.
test_directDotfileRequestRefused :: Assertion
test_directDotfileRequestRefused =
  withSystemTempDirectory "venusia-direct-dotfile" $ \dir -> do
    writeFile (dir </> ".env") "SECRET=correct-horse-battery-staple\n"
    let routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/.env\r\n"
      assertBool "direct dotfile request refused"
                 (BS.isInfixOf "Access denied" resp)
      assertBool "dotfile content does NOT appear in the response"
                 (not (BS.isInfixOf "correct-horse-battery-staple" resp))

-- | The configured directory-menu index file (default @.gophermap@) is
-- the one dotfile name the framework treats as content by convention,
-- and a direct request for it must succeed even with the default
-- @allowDotfiles=False@. The narrow exemption is final-component-only:
-- @/foo/.gophermap@ is allowed; @/foo/.env@ is still refused (covered
-- by 'test_directDotfileRequestRefused').
test_directIndexFileRequestAllowed :: Assertion
test_directIndexFileRequestAllowed =
  withSystemTempDirectory "venusia-direct-indexfile" $ \dir -> do
    writeFile (dir </> ".gophermap") "iindex-file-marker\t\t\t0\r\n.\r\n"
    let routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/.gophermap\r\n"
      assertBool "direct .gophermap request returns the file (not refused)"
                 (BS.isInfixOf "index-file-marker" resp)
      assertBool "no access-denied for the index file"
                 (not (BS.isInfixOf "Access denied" resp))

-- | The index-file exemption is narrow: only the FINAL path component
-- gets the pass. A request like @/foo/.git/config@ — where @.git@ is
-- an intermediate dotfile component (a VCS directory) and @config@
-- is its inner config file — must still be refused. Browsing /through/
-- a dotfile leaks structure that's almost always sensitive.
test_intermediateDotfileRefused :: Assertion
test_intermediateDotfileRefused =
  withSystemTempDirectory "venusia-intermediate-dotfile" $ \dir -> do
    createDirectoryIfMissing True (dir </> ".git")
    writeFile (dir </> ".git" </> "config") "[user]\n  email = secret@example.com\n"
    let routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectory "127.0.0.1" 7070 dir "" wp Nothing
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/.git/config\r\n"
      assertBool "intermediate-dotfile traversal refused"
                 (BS.isInfixOf "Access denied" resp)
      assertBool "VCS config content does NOT appear in the response"
                 (not (BS.isInfixOf "secret@example.com" resp))

-- | Opt-in: a block with allowDotfiles=True both enumerates and
-- serves dotfiles. Used by `[[files]]` configs with
-- @allow_dotfiles = true@ — for the dotfiles-as-content edge case.
test_listingShowsDotfilesWhenOptedIn :: Assertion
test_listingShowsDotfilesWhenOptedIn =
  withSystemTempDirectory "venusia-listing-showdots" $ \dir -> do
    writeFile (dir </> "real.txt")    "visible\n"
    writeFile (dir </> ".bashrc")     "dotfile content\n"
    let hook       = \_ -> pure Nothing
        itemTypeFn = fileExtensionToItemType
        routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "" wp Nothing
                    hook itemTypeFn True ".gophermap" []  -- opted in
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      -- Listing visibility
      listResp <- gopherRoundtrip port "/\r\n"
      assertBool "visible file is enumerated"
                 (BS.isInfixOf "real.txt" listResp)
      assertBool ".bashrc is also enumerated when allowDotfiles=True"
                 (BS.isInfixOf ".bashrc" listResp)
      -- Direct-access servability
      directResp <- gopherRoundtrip port "/.bashrc\r\n"
      assertBool "direct dotfile request served when allowDotfiles=True"
                 (BS.isInfixOf "dotfile content" directResp)

-- | The @unlisted@ field on a [[files]] block hides matching files from
-- the auto-generated listing but does NOT block direct fetches by
-- exact selector. Distinct from the dotfile guard, which gates both.
test_unlistedFiltersListingNotDirect :: Assertion
test_unlistedFiltersListingNotDirect =
  withSystemTempDirectory "venusia-listing-unlisted" $ \dir -> do
    writeFile (dir </> "hello.txt")     "visible\n"
    writeFile (dir </> "bartleby.conf") "hostname: example.com\n"
    writeFile (dir </> "foo.bcard")     "title: foo\n"
    writeFile (dir </> "bar.bcard")     "title: bar\n"
    let hook       = \_ -> pure Nothing
        itemTypeFn = fileExtensionToItemType
        routes =
          [ onWildcard "" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "" wp Nothing
                    hook itemTypeFn False ".gophermap"
                    ["bartleby.conf", "*.bcard"]
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      -- Listing: only the non-unlisted file appears.
      listResp <- gopherRoundtrip port "/\r\n"
      assertBool "non-unlisted file appears in the listing"
                 (BS.isInfixOf "hello.txt" listResp)
      assertBool "exact-match unlisted entry hidden from listing"
                 (not (BS.isInfixOf "bartleby.conf" listResp))
      assertBool "glob-matched .bcard entries hidden from listing"
                 (not (BS.isInfixOf ".bcard" listResp))
      -- Direct fetch: unlisted files still served by exact selector.
      bartResp <- gopherRoundtrip port "/bartleby.conf\r\n"
      assertBool "direct fetch of exact-unlisted file returns file body"
                 (BS.isInfixOf "hostname: example.com" bartResp)
      bcardResp <- gopherRoundtrip port "/foo.bcard\r\n"
      assertBool "direct fetch of glob-unlisted file returns file body"
                 (BS.isInfixOf "title: foo" bcardResp)

------------------------------------------------------------------------
-- Preamble/postamble framing
------------------------------------------------------------------------

-- | When 'asInfoLines = True', preamble and postamble entries that lack
-- a trailing CRLF must be auto-terminated. Otherwise the gopher
-- terminator (".\\r\\n") glues onto the tail of the last item.
test_preamblePostambleCRLF :: Assertion
test_preamblePostambleCRLF = do
  let pre  = ["1Header\theader\thost\t70"]      -- no trailing CRLF
      post = ["7More\tmore\thost\t70"]          -- no trailing CRLF
      handler _ = runProcess False True "/bin/sh"
                    ["-c", "printf 'middle\\n'"]
                    Nothing pre post
      routes = [ on "/framed" handler ]
  withServer routes $ \port -> do
    resp <- gopherRoundtrip port "/framed\r\n"
    -- Each of the four items (preamble, info-wrapped 'middle', postamble,
    -- terminator) must be on its own line.
    resp @?= "1Header\theader\thost\t70\r\n\
             \imiddle\t\t\t0\r\n\
             \7More\tmore\thost\t70\r\n\
             \.\r\n"

------------------------------------------------------------------------
-- Disconnect kills the streaming child
------------------------------------------------------------------------

-- | The most load-bearing claim about streaming: when a client disconnects
-- mid-stream, the server's bracket cleanup terminates the running process
-- so it can't keep consuming resources.
--
-- We launch a streaming script that writes 'line' both to stdout (so the
-- streaming send path runs and a broken pipe surfaces quickly) and to a
-- tempfile we control (the side channel we sample). Read a few bytes,
-- close the connection, then verify the tempfile size stops growing.
test_streamingDisconnectKillsChild :: Assertion
test_streamingDisconnectKillsChild =
  withSystemTempFile "venusia-disconnect-" $ \tempfile h -> do
    hClose h
    let script =
          "while :; do echo line >> '" <> tempfile
            <> "'; echo line; sleep 0.05; done"
        handler _ = runProcess True False "/bin/sh" ["-c", script]
                      Nothing [] []
        routes = [ on "/loop" handler ]
    withServer routes $ \port -> do
      -- Connect, send request, read a chunk to confirm the script is
      -- live, then close. The bracketed close fires regardless of how
      -- the inner action exits.
      bracket
        (do
          addr:_ <- getAddrInfo
            (Just defaultHints { addrSocketType = Stream })
            (Just "127.0.0.1") (Just (show port))
          s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect s (addrAddress addr)
          NBS.sendAll s "/loop\r\n"
          _ <- NBS.recv s 16
          pure s)
        (\s -> do
          -- 'shutdown' before 'close' tears the connection down promptly:
          -- a plain close() emits a FIN, after which Linux happily buffers
          -- further sends from the server until eventual delivery failure
          -- or TCP_USER_TIMEOUT (120 s). 'shutdown ShutdownBoth' surfaces
          -- the peer-gone state to the server's next send() within a
          -- round-trip on loopback, so cleanup fires while the test is
          -- still running.
          shutdown s ShutdownBoth `catch` (\(_ :: SomeException) -> pure ())
          close s)
        (\_ -> pure ())
      -- Give the server time to detect the broken pipe, run cleanup,
      -- send SIGTERM, and reap. Generous: includes the 2 s grace period
      -- before SIGKILL would fire (which a /bin/sh script honours
      -- immediately).
      threadDelay 1_000_000
      size1 <- BS.length <$> BS.readFile tempfile
      threadDelay 1_000_000
      size2 <- BS.length <$> BS.readFile tempfile
      let growth = size2 - size1
      assertBool ("script wrote at least once before disconnect (size1="
                   <> show size1 <> ")")
                 (size1 > 0)
      -- Tolerance: a single in-flight iteration may flush a few bytes
      -- after SIGTERM. 50 bytes ~= 10 iterations of "line\n", which is
      -- way more than any post-kill flush should produce.
      assertBool ("script kept running after disconnect (growth="
                   <> show growth <> " bytes in 1s)")
                 (growth < 50)

------------------------------------------------------------------------
-- Substitution contract for [[script_extension]]
--
-- These tests pin the agreed surface: $file and $search are substituted;
-- nothing else is. Locking this in means future "harmless" additions of
-- $selector / $wildcard / $host / $port will fail a test rather than
-- silently widen the contract.
------------------------------------------------------------------------

-- | Positive: when a request carries a query, '$search' in the script
-- extension's argument template is replaced with that query before the
-- command is invoked.
test_scriptExtensionSearchSubstitution :: Assertion
test_scriptExtensionSearchSubstitution =
  withSystemTempDirectory "venusia-subst-search" $ \dir -> do
    let filePath = dir </> "marker.sh"
    writeFile filePath ""  -- existence is what triggers the hook
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/echo"
              , arguments   = ["search-was:", "$search"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              -- Mirror the per-request hook construction that
              -- 'createFileHandler' performs in production.
              let hook = mkScriptHook scriptExts req.reqSelector req.reqQuery "" req.reqClientIp
              in case req.reqWildcard of
                   Just wp ->
                     serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp
                       Nothing hook itemTypeFn False ".gophermap" []
                   Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/marker.sh\thello-world\r\n"
      assertBool "$search substituted to the request query"
                 (BS.isInfixOf "search-was: hello-world" resp)

-- | $selector substitutes with the gopher selector that resolved to the
-- script. Lets a script generate menu items pointing back at itself
-- without hardcoding its own path — which matters when a script's
-- selector can change ([[files]] mount renames, dir reorganisation).
test_scriptExtensionSelectorSubstitution :: Assertion
test_scriptExtensionSelectorSubstitution =
  withSystemTempDirectory "venusia-subst-selector" $ \dir -> do
    let filePath = dir </> "marker.sh"
    writeFile filePath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/echo"
              , arguments   = ["selector-was:", "$selector"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              -- Mirror createFileHandler: build the hook per-request so
              -- it captures req.reqSelector.
              let hook = mkScriptHook scriptExts req.reqSelector req.reqQuery "" req.reqClientIp
              in case req.reqWildcard of
                   Just wp ->
                     serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp
                       Nothing hook itemTypeFn False ".gophermap" []
                   Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/marker.sh\r\n"
      assertBool "$selector substituted with the request's selector"
                 (BS.isInfixOf "selector-was: /files/marker.sh" resp)

-- | $remote_ip carries the peer address (text form) through to the
-- script. The test server binds to 127.0.0.1, so the substitution
-- result is the loopback literal. Empty when the peer can't be looked
-- up — but the local connection here is fine, so we assert presence.
test_scriptExtensionRemoteIpSubstitution :: Assertion
test_scriptExtensionRemoteIpSubstitution =
  withSystemTempDirectory "venusia-subst-remoteip" $ \dir -> do
    let filePath = dir </> "marker.sh"
    writeFile filePath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/echo"
              , arguments   = ["remote-ip:", "$remote_ip"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              let hook = mkScriptHook scriptExts req.reqSelector req.reqQuery "" req.reqClientIp
              in case req.reqWildcard of
                   Just wp ->
                     serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp
                       Nothing hook itemTypeFn False ".gophermap" []
                   Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/marker.sh\r\n"
      assertBool "$remote_ip substituted with the loopback peer"
                 (BS.isInfixOf "remote-ip: 127.0.0.1" resp)

-- | Negative: a token /not/ part of the documented substitution surface
-- (currently $file, $selector, $search, $pathinfo) is passed through
-- verbatim. Guards against later "harmless" additions silently consuming
-- a literal string a script-author intended to receive as-is.
test_scriptExtensionTokenPassthrough :: Assertion
test_scriptExtensionTokenPassthrough =
  withSystemTempDirectory "venusia-subst-passthrough" $ \dir -> do
    let filePath = dir </> "marker.sh"
    writeFile filePath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/echo"
              , arguments   = ["got:", "$wildcard"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        hook       = mkScriptHook scriptExts "/files/marker.sh" Nothing "" ""
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/marker.sh\r\n"
      -- $wildcard is not in script_extension's substitution surface
      -- (per substituteScriptArg's documented list) — only $file,
      -- $selector, $search, $pathinfo are.
      assertBool "unrecognized $-token reaches the script verbatim"
                 (BS.isInfixOf "$wildcard" resp)

-- | Path-info: a request like @/files/wiki.lhs/Page/SubPage@ executes
-- @wiki.lhs@ and threads @/Page/SubPage@ into @$pathinfo@. The route
-- wiring below mirrors what 'createFileHandler' does in production:
-- 'splitForPathInfo' picks the script-prefix\/path-info split, then
-- 'mkScriptHook' is built with the carved-out suffix.
test_scriptExtensionPathInfo :: Assertion
test_scriptExtensionPathInfo =
  withSystemTempDirectory "venusia-pathinfo" $ \dir -> do
    let scriptPath = dir </> "wiki.lhs"
    writeFile scriptPath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "lhs"
              , command     = "/bin/echo"
              , arguments   = ["pathinfo:", "$pathinfo"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp -> do
                  (sw, pInfo) <- splitForPathInfo dir scriptExts wp
                  let hook = mkScriptHook scriptExts req.reqSelector
                                          req.reqQuery pInfo req.reqClientIp
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" sw
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/wiki.lhs/Page/SubPage\r\n"
      assertBool "$pathinfo carries the suffix"
                 (BS.isInfixOf "pathinfo: /Page/SubPage" resp)

-- | Path-info empty case: when the request addresses the script
-- directly with no trailing components, @$pathinfo@ is the empty
-- string. @\/bin\/echo@ collapses adjacent argument boundaries, so the
-- output reads @\"pathinfo: \\n\"@ (token gone, separator preserved).
test_scriptExtensionPathInfoEmpty :: Assertion
test_scriptExtensionPathInfoEmpty =
  withSystemTempDirectory "venusia-pathinfo-empty" $ \dir -> do
    let scriptPath = dir </> "wiki.lhs"
    writeFile scriptPath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "lhs"
              , command     = "/bin/echo"
              , arguments   = ["pathinfo:[", "$pathinfo", "]end"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp -> do
                  (sw, pInfo) <- splitForPathInfo dir scriptExts wp
                  let hook = mkScriptHook scriptExts req.reqSelector
                                          req.reqQuery pInfo req.reqClientIp
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" sw
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/wiki.lhs\r\n"
      -- The middle token expands to "" so we see the brackets touching
      -- a single space (echo joins args with one space).
      assertBool "$pathinfo is empty when no suffix is present"
                 (BS.isInfixOf "pathinfo:[  ]end" resp)

-- | Path-info trailing-slash distinction: @\/cgi\/wiki.lhs\/@ is treated
-- as path-info @\/@ (not empty), matching CGI's PATH_INFO convention so
-- the script can tell "addressed with trailing slash" from "addressed
-- without".
test_scriptExtensionPathInfoTrailingSlash :: Assertion
test_scriptExtensionPathInfoTrailingSlash =
  withSystemTempDirectory "venusia-pathinfo-slash" $ \dir -> do
    let scriptPath = dir </> "wiki.lhs"
    writeFile scriptPath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "lhs"
              , command     = "/bin/echo"
              , arguments   = ["pathinfo:[", "$pathinfo", "]end"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp -> do
                  (sw, pInfo) <- splitForPathInfo dir scriptExts wp
                  let hook = mkScriptHook scriptExts req.reqSelector
                                          req.reqQuery pInfo req.reqClientIp
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" sw
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/wiki.lhs/\r\n"
      assertBool "$pathinfo is '/' for a bare trailing slash"
                 (BS.isInfixOf "pathinfo:[ / ]end" resp)

-- | When the segment ending in a registered script extension does NOT
-- correspond to a real file (e.g. @nope.lhs@ doesn't exist), the wildcard
-- is /not/ split: the request falls through to ordinary file resolution
-- and the script never runs. Guards against speculative execution on
-- mistyped paths and against directories whose name happens to end with
-- a registered extension.
test_scriptExtensionPathInfoNoFile :: Assertion
test_scriptExtensionPathInfoNoFile =
  withSystemTempDirectory "venusia-pathinfo-nofile" $ \dir -> do
    -- Deliberately create no .lhs file. Any execution would be a bug.
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "lhs"
              , command     = "/bin/echo"
              , arguments   = ["should-not-run"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp -> do
                  (sw, pInfo) <- splitForPathInfo dir scriptExts wp
                  let hook = mkScriptHook scriptExts req.reqSelector
                                          req.reqQuery pInfo req.reqClientIp
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" sw
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/nope.lhs/foo\r\n"
      assertBool "no script execution when prefix doesn't resolve to a real file"
                 (not (BS.isInfixOf "should-not-run" resp))

-- | Path-info and $search are independent. A request like
-- @/files/wiki.lhs/Page\\tquery@ populates @$pathinfo = \"\/Page\"@ and
-- @$search = \"query\"@ — neither one bleeds into the other.
test_scriptExtensionPathInfoSearchCoexists :: Assertion
test_scriptExtensionPathInfoSearchCoexists =
  withSystemTempDirectory "venusia-pathinfo-search" $ \dir -> do
    let scriptPath = dir </> "wiki.lhs"
    writeFile scriptPath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "lhs"
              , command     = "/bin/echo"
              , arguments   = ["pi=", "$pathinfo", "q=", "$search"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp -> do
                  (sw, pInfo) <- splitForPathInfo dir scriptExts wp
                  let hook = mkScriptHook scriptExts req.reqSelector
                                          req.reqQuery pInfo req.reqClientIp
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" sw
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/wiki.lhs/Page\thello\r\n"
      assertBool "$pathinfo and $search are populated independently"
                 (BS.isInfixOf "pi= /Page q= hello" resp)

-- | Defence in depth around the bounds check: a wildcard whose script-prefix
-- segment, after canonicalisation, escapes the served root must never run.
-- 'splitForPathInfo' rejects the candidate at its own bounds check, the
-- wildcard then falls through to 'serveDirectoryWith' unsplit, and that
-- function's bounds check rejects the request with an access-denied
-- response.
test_scriptExtensionPathInfoTraversalGuard :: Assertion
test_scriptExtensionPathInfoTraversalGuard =
  withSystemTempDirectory "venusia-pathinfo-traversal" $ \parent -> do
    -- Lay out a sibling .lhs file *outside* the served root: served root is
    -- parent/served, the lure is parent/escape.lhs. A naive split would
    -- attempt to execute it.
    let servedRoot = parent </> "served"
        outsider   = parent </> "escape.lhs"
    createDirectoryIfMissing True servedRoot
    writeFile outsider ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "lhs"
              , command     = "/bin/echo"
              , arguments   = ["should-not-run"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp -> do
                  (sw, pInfo) <- splitForPathInfo servedRoot scriptExts wp
                  let hook = mkScriptHook scriptExts req.reqSelector
                                          req.reqQuery pInfo req.reqClientIp
                  serveDirectoryWith "127.0.0.1" 7070 servedRoot "/files/" sw
                    Nothing hook itemTypeFn False ".gophermap" []
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/../escape.lhs/x\r\n"
      assertBool "outside-root script not executed"
                 (not (BS.isInfixOf "should-not-run" resp))
      assertBool "request rejected with access-denied response"
                 (BS.isInfixOf "Access denied" resp)

-- | Verifies tomland actually parses nested array-of-tables for
-- [[files.script_extension]] and [[files.file_type]]. This is the
-- foundational claim behind the 0.7.0.0 schema redesign; if it doesn't
-- decode, the whole thing fails.
test_tomlandNestedArrayDecode :: Assertion
test_tomlandNestedArrayDecode = do
  let src =
        "[[files]]\n\
        \selector = \"/cgi/\"\n\
        \path = \"/var/gopher/output/cgi/\"\n\
        \\n\
        \  [[files.script_extension]]\n\
        \  extension = \"lhs\"\n\
        \  command = \"runghc\"\n\
        \  arguments = [\"$file\", \"$selector\", \"$search\"]\n\
        \\n\
        \  [[files.file_type]]\n\
        \  extension = \"lhs\"\n\
        \  item_type = \"1\"\n\
        \\n\
        \[[files]]\n\
        \selector = \"\"\n\
        \path = \"/var/gopher/output\"\n\
        \\n\
        \[[file_type]]\n\
        \extension = \"md\"\n\
        \item_type = \"0\"\n"
  case Toml.decode routesConfigCodec src of
    Left err -> assertFailure ("tomland failed to decode nested config: " ++ show err)
    Right cfg -> do
      length cfg.files                                                @?= 2
      let cgiFiles = head cfg.files
      cgiFiles.selector                                               @?= "/cgi/"
      length cgiFiles.scriptExtensions                                @?= 1
      (head cgiFiles.scriptExtensions).extension                      @?= "lhs"
      (head cgiFiles.scriptExtensions).command                        @?= "runghc"
      (head cgiFiles.scriptExtensions).arguments                      @?= ["$file", "$selector", "$search"]
      length cgiFiles.fileTypes                                       @?= 1
      (head cgiFiles.fileTypes).extension                             @?= "lhs"
      (head cgiFiles.fileTypes).itemType                              @?= "1"
      let rootFiles = cfg.files !! 1
      rootFiles.selector                                              @?= ""
      length rootFiles.scriptExtensions                               @?= 0
      length rootFiles.fileTypes                                      @?= 0
      length cfg.fileTypes                                            @?= 1
      (head cfg.fileTypes).extension                                  @?= "md"
