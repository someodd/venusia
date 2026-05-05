module Test.Venusia.MenuBuilder (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.Instances ()  -- Arbitrary for Text

import qualified Data.Text as T

import Venusia.MenuBuilder
  ( item
  , menu
  , render
  , info
  , error'
  , gophermapRender
  )

tests :: TestTree
tests = testGroup "Venusia.MenuBuilder"
  [ itemTests
  , menuTests
  , infoErrorTests
  , gophermapRenderTests
  ]

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

-- | Text guaranteed not to contain any of the structural characters used by
-- the Gopher line format, so we can reason about positions and counts.
newtype CleanText = CleanText T.Text deriving Show

instance Arbitrary CleanText where
  arbitrary = CleanText . T.pack <$> listOf (arbitrary `suchThat` clean)
    where clean c = c /= '\t' && c /= '\r' && c /= '\n'
  shrink (CleanText t) =
    [ CleanText (T.pack s)
    | s <- shrink (T.unpack t)
    , all (\c -> c /= '\t' && c /= '\r' && c /= '\n') s
    ]

-- | A Gopher item type character (we exercise the published set; @item@
-- accepts any @Char@ but these are what real menus use).
newtype TypeChar = TypeChar Char deriving Show

instance Arbitrary TypeChar where
  arbitrary = TypeChar <$> elements "01345679gIih"

-- | A reasonable port number.
newtype Port = Port Int deriving Show

instance Arbitrary Port where
  arbitrary = Port <$> choose (1, 65535)

------------------------------------------------------------------------
-- item
------------------------------------------------------------------------

itemTests :: TestTree
itemTests = testGroup "item"
  [ testProperty "starts with the type character" $
      \(TypeChar t) (CleanText d) (CleanText s) (CleanText h) (Port p) ->
        let line = item t d s h p
        in not (T.null line) ==> T.head line === t

  , testProperty "ends with CRLF" $
      \(TypeChar t) (CleanText d) (CleanText s) (CleanText h) (Port p) ->
        T.takeEnd 2 (item t d s h p) === "\r\n"

  , testProperty "contains exactly three tabs" $
      \(TypeChar t) (CleanText d) (CleanText s) (CleanText h) (Port p) ->
        T.count "\t" (item t d s h p) === 3

  , testProperty "fields appear in display \\t selector \\t host \\t port order" $
      \(TypeChar t) (CleanText d) (CleanText s) (CleanText h) (Port p) ->
        let line = T.dropEnd 2 (item t d s h p)  -- drop CRLF
            firstColon = T.drop 1 line  -- drop type char
            parts = T.splitOn "\t" firstColon
        in parts === [d, s, h, T.pack (show p)]

  , testCase "well-known smoke vector" $
      item '0' "Hello" "/hello" "host" 70 @?= "0Hello\t/hello\thost\t70\r\n"
  ]

------------------------------------------------------------------------
-- menu / render
------------------------------------------------------------------------

menuTests :: TestTree
menuTests = testGroup "menu / render"
  [ testCase "empty menu is just the terminator" $
      menu [] @?= ".\r\n"

  , testCase "render is alias for menu" $
      render ["x"] @?= menu ["x"]

  , testProperty "menu always ends with the . terminator" $ \xs ->
      T.takeEnd 3 (menu xs) === ".\r\n"

  , testProperty "menu = concat items <> terminator" $ \xs ->
      menu xs === T.concat xs <> ".\r\n"
  ]

------------------------------------------------------------------------
-- info / error'
------------------------------------------------------------------------

infoErrorTests :: TestTree
infoErrorTests = testGroup "info / error'"
  [ testProperty "info line starts with 'i'" $ \(CleanText msg) ->
      not (T.null (info msg)) ==> T.head (info msg) === 'i'

  , testProperty "error' line starts with '3'" $ \(CleanText msg) ->
      not (T.null (error' msg)) ==> T.head (error' msg) === '3'

  , testCase "info shape" $
      info "hello" @?= "ihello\t\t\t0\r\n"

  , testCase "error' shape" $
      error' "boom" @?= "3boom\t\t\t0\r\n"
  ]

------------------------------------------------------------------------
-- gophermapRender
------------------------------------------------------------------------

gophermapRenderTests :: TestTree
gophermapRenderTests = testGroup "gophermapRender"
  [ testCase "info-style line gets host/port filled in" $
      let out = gophermapRender "host" 70 "0Hello\t/hello"
      in out @?= "0Hello\t/hello\thost\t70\r\n.\r\n"

  , testCase "fully-qualified line is preserved" $
      let out = gophermapRender "host" 70 "1Dir\t/dir\tother\t7070"
      in out @?= "1Dir\t/dir\tother\t7070\r\n.\r\n"

  , testCase "plain text line becomes an info item" $
      let out = gophermapRender "host" 70 "just a comment"
      in out @?= "ijust a comment\t\t\t0\r\n.\r\n"

  , testCase "h-type with URL: prefix passes through" $
      let out = gophermapRender "host" 70 "hExample\tURL:http://example.com"
      in out @?= "hExample\tURL:http://example.com\thost\t70\r\n.\r\n"

  , testCase "always ends with the . terminator" $
      let out = gophermapRender "host" 70 "0a\t/a\n1b\t/b"
      in T.takeEnd 3 out @?= ".\r\n"
  ]
