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
import Control.Exception (bracket, catch, try, SomeException)
import Control.Monad (forM_, replicateM_)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)

import Venusia.FileHandler (serveDirectory, serveDirectoryWith)
import Venusia.Server
  ( Route(..)
  , Request(..)
  , Response(..)
  , noMatchHandler
  , on
  , onWildcard
  , runOnSocket
  )
import Venusia.Routes
  ( ScriptExtensionConfig(..)
  , FileTypeConfig(..)
  , mkScriptHook
  , resolveItemType
  , runProcess
  )

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
  , testCase "preamble/postamble lines auto-terminated with CRLF in info-wrap mode"
                                                             test_preamblePostambleCRLF
  , testCase "streaming child process is killed when client disconnects mid-stream"
                                                             test_streamingDisconnectKillsChild
  , testCase "script_extension $search is substituted from the request query"
                                                             test_scriptExtensionSearchSubstitution
  , testCase "script_extension unrecognized tokens pass through verbatim"
                                                             test_scriptExtensionTokenPassthrough
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
      tid <- forkIO $ runOnSocket sock noMatchHandler routesVar
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
        hook       = mkScriptHook scriptExts Nothing
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp Nothing
                    hook itemTypeFn
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
        hook       = mkScriptHook scriptExts Nothing
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp Nothing
                    hook itemTypeFn
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
        hook       = mkScriptHook scriptExts Nothing
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp Nothing
                    hook itemTypeFn
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
              let hook = mkScriptHook scriptExts req.reqQuery
              in case req.reqWildcard of
                   Just wp ->
                     serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp
                       Nothing hook itemTypeFn
                   Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/marker.sh\thello-world\r\n"
      assertBool "$search substituted to the request query"
                 (BS.isInfixOf "search-was: hello-world" resp)

-- | Negative: a token that is /not/ part of the documented substitution
-- surface (e.g. '$selector') is passed through verbatim. This guards
-- against a later "harmless" addition silently consuming a literal
-- string a script-author intended to receive as-is.
test_scriptExtensionTokenPassthrough :: Assertion
test_scriptExtensionTokenPassthrough =
  withSystemTempDirectory "venusia-subst-passthrough" $ \dir -> do
    let filePath = dir </> "marker.sh"
    writeFile filePath ""
    let scriptExts =
          [ ScriptExtensionConfig
              { extension   = "sh"
              , command     = "/bin/echo"
              , arguments   = ["got:", "$selector"]
              , stream      = Nothing
              , asInfoLines = Nothing
              }
          ]
        hook       = mkScriptHook scriptExts Nothing
        itemTypeFn = resolveItemType [] scriptExts
        routes =
          [ onWildcard "/files/*" $ \req ->
              case req.reqWildcard of
                Just wp ->
                  serveDirectoryWith "127.0.0.1" 7070 dir "/files/" wp
                    Nothing hook itemTypeFn
                Nothing -> pure (TextResponse "no path")
          ]
    withServer routes $ \port -> do
      resp <- gopherRoundtrip port "/files/marker.sh\r\n"
      -- The literal '$selector' must reach the script's stdout. If it
      -- doesn't, someone has expanded the substitution surface without
      -- updating the docs and tests.
      assertBool "unrecognized $-token reaches the script verbatim"
                 (BS.isInfixOf "$selector" resp)
