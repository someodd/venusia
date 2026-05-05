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
import Control.Exception (bracket, try, SomeException)
import Control.Monad (forM_, replicateM_)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

import Venusia.Server
  ( Route(..)
  , Request(..)
  , Response(..)
  , noMatchHandler
  , on
  , runOnSocket
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
