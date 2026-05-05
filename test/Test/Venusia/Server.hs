module Test.Venusia.Server (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Instances ()  -- Arbitrary for Text, ByteString

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Word (Word8)

import Venusia.Server
  ( parseRequest
  , sanitizeSelector
  , on
  , onWildcard
  , Route(..)
  , Request(..)
  , Handler
  , Response(..)
  )

tests :: TestTree
tests = testGroup "Venusia.Server"
  [ sanitizeSelectorTests
  , parseRequestTests
  , onTests
  , onWildcardTests
  ]

------------------------------------------------------------------------
-- sanitizeSelector
------------------------------------------------------------------------

-- | A ByteString that may contain CR/LF/whitespace; useful for stress-testing
-- the sanitizer.
newtype Bytes = Bytes BS.ByteString deriving Show

instance Arbitrary Bytes where
  arbitrary = Bytes . BS.pack <$> listOf (elements byteAlphabet)
    where
      -- Mix of normal chars, CR, LF, TAB, and surrounding whitespace so
      -- properties hit the interesting branches frequently.
      byteAlphabet :: [Word8]
      byteAlphabet =
        [ 0x20, 0x21, 0x23, 0x2F, 0x41, 0x42, 0x43, 0x61, 0x7A
        , 0x09  -- tab
        , 0x0A  -- LF
        , 0x0D  -- CR
        ]
  shrink (Bytes b) = Bytes <$> shrink b

sanitizeSelectorTests :: TestTree
sanitizeSelectorTests = testGroup "sanitizeSelector"
  [ testProperty "is idempotent" $ \(Bytes b) ->
      sanitizeSelector (sanitizeSelector b) === sanitizeSelector b

  , testProperty "result has no CR" $ \(Bytes b) ->
      BS8.elem '\r' (sanitizeSelector b) === False

  , testProperty "result has no LF" $ \(Bytes b) ->
      BS8.elem '\n' (sanitizeSelector b) === False

  , testProperty "result is no longer than input" $ \(Bytes b) ->
      BS.length (sanitizeSelector b) <= BS.length b

  , testProperty "truncates at first CRLF (anything after \\r is dropped)" $ \(Bytes a) (Bytes b) ->
      let cleanA = BS8.takeWhile (\c -> c /= '\r' && c /= '\n') a
      in sanitizeSelector (cleanA <> "\r\n" <> b) === sanitizeSelector cleanA

  , testProperty "truncates at first bare LF" $ \(Bytes a) (Bytes b) ->
      let cleanA = BS8.takeWhile (\c -> c /= '\r' && c /= '\n') a
      in sanitizeSelector (cleanA <> "\n" <> b) === sanitizeSelector cleanA

  , testCase "empty input → empty" $
      sanitizeSelector "" @?= ""

  , testCase "trailing CRLF stripped" $
      sanitizeSelector "/foo\r\n" @?= "/foo"

  , testCase "trailing whitespace stripped" $
      sanitizeSelector "  /foo  " @?= "/foo"

  , testCase "embedded CR drops the rest of the line" $
      sanitizeSelector "/foo\r\nGET /bar\r\n" @?= "/foo"

  , testCase "tab is preserved (used to separate selector from query)" $
      sanitizeSelector "/foo\tquery\r\n" @?= "/foo\tquery"
  ]

------------------------------------------------------------------------
-- parseRequest
------------------------------------------------------------------------

-- | Text without any tabs — the natural fixture for selector / query parts.
newtype TabFree = TabFree T.Text deriving Show

instance Arbitrary TabFree where
  arbitrary = TabFree . T.pack <$> listOf (arbitrary `suchThat` (/= '\t'))
  shrink (TabFree t) = TabFree . T.pack <$> shrink (T.unpack t)

parseRequestTests :: TestTree
parseRequestTests = testGroup "parseRequest"
  [ testProperty "input without tabs returns (input, Nothing)" $ \(TabFree t) ->
      parseRequest t === (t, Nothing)

  , testProperty "selector\\tquery → (selector, Just query)" $ \(TabFree s) (TabFree q) ->
      parseRequest (s <> "\t" <> q) === (s, Just q)

  , testProperty "extra tabs after the first are dropped" $
      \(TabFree a) (TabFree b) (TabFree c) ->
        parseRequest (a <> "\t" <> b <> "\t" <> c) === (a, Just b)

  , testCase "empty input" $
      parseRequest "" @?= ("", Nothing)

  , testCase "trailing tab → query is empty Text" $
      parseRequest "/foo\t" @?= ("/foo", Just "")

  , testCase "leading tab → selector is empty" $
      parseRequest "\tquery" @?= ("", Just "query")
  ]

------------------------------------------------------------------------
-- on / onWildcard
------------------------------------------------------------------------

-- | Stub handler so we can build Routes; never actually invoked by the matcher.
stubHandler :: Handler
stubHandler _ = pure (TextResponse "")

onTests :: TestTree
onTests = testGroup "on"
  [ testProperty "exact selector matches" $ \(TabFree path) ->
      let r = on path stubHandler
      in case r.matchRoute path of
           Just req -> req.reqSelector === path
           Nothing  -> counterexample "expected match" False

  , testProperty "different selector does not match" $ \(TabFree path) (TabFree other) ->
      path /= other ==>
        (on path stubHandler).matchRoute other === Nothing

  , testProperty "query is preserved through matching" $ \(TabFree path) (TabFree q) ->
      let r = on path stubHandler
      in case r.matchRoute (path <> "\t" <> q) of
           Just req -> req.reqQuery === Just q
           Nothing  -> counterexample "expected match" False
  ]

onWildcardTests :: TestTree
onWildcardTests = testGroup "onWildcard"
  [ testCase "captures middle segment" $ do
      let r = onWildcard "/foo/*/bar" stubHandler
      case r.matchRoute "/foo/MIDDLE/bar" of
        Just req -> req.reqWildcard @?= Just "MIDDLE"
        Nothing  -> assertFailure "expected match"

  , testCase "captures trailing path" $ do
      let r = onWildcard "/files/*" stubHandler
      case r.matchRoute "/files/a/b/c.txt" of
        Just req -> req.reqWildcard @?= Just "a/b/c.txt"
        Nothing  -> assertFailure "expected match"

  , testCase "rejects mismatched suffix" $ do
      let r = onWildcard "/foo/*/bar" stubHandler
      r.matchRoute "/foo/MIDDLE/baz" @?= Nothing

  , testCase "rejects mismatched prefix" $ do
      let r = onWildcard "/foo/*/bar" stubHandler
      r.matchRoute "/qux/MIDDLE/bar" @?= Nothing

  , testProperty "captured wildcard plus prefix/suffix reconstructs the input" $
      \(TabFree mid) ->
        not (T.null mid) && not (T.any (== '\t') mid) ==>
        let r = onWildcard "/p/*/s" stubHandler
            sel = "/p/" <> mid <> "/s"
        in case r.matchRoute sel of
             Just req ->
               req.reqWildcard === Just mid
               .&&. req.reqSelector === sel
             Nothing -> counterexample "expected match" False
  ]
