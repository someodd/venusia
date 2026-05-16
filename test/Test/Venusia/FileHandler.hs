module Test.Venusia.FileHandler (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import Venusia.FileHandler (matchGlob)

tests :: TestTree
tests = testGroup "Venusia.FileHandler"
  [ matchGlobTests
  ]

------------------------------------------------------------------------
-- matchGlob
------------------------------------------------------------------------

matchGlobTests :: TestTree
matchGlobTests = testGroup "matchGlob"
  -- No '*' → exact match only
  [ testCase "exact match"          $ matchGlob "foo" "foo" @?= True
  , testCase "exact mismatch (extra suffix)" $
      matchGlob "foo" "foobar" @?= False
  , testCase "exact mismatch (extra prefix)" $
      matchGlob "foo" "xfoo" @?= False
  , testCase "empty pattern matches only empty name" $ do
      matchGlob "" "" @?= True
      matchGlob "" "x" @?= False

  -- '*' at start (suffix-matching glob)
  , testCase "*.bcard matches a.bcard"     $ matchGlob "*.bcard" "a.bcard" @?= True
  , testCase "*.bcard matches applets.bcard" $
      matchGlob "*.bcard" "applets.bcard" @?= True
  , testCase "*.bcard does NOT match bcard"  $
      matchGlob "*.bcard" "bcard" @?= False
  , testCase "*.bcard does NOT match foo.bcard.bak" $
      matchGlob "*.bcard" "foo.bcard.bak" @?= False

  -- '*' at end (prefix-matching glob)
  , testCase "bartleby.* matches bartleby.conf" $
      matchGlob "bartleby.*" "bartleby.conf" @?= True
  , testCase "bartleby.* matches bartleby.yaml" $
      matchGlob "bartleby.*" "bartleby.yaml" @?= True
  , testCase "bartleby.* does NOT match bartleby" $
      matchGlob "bartleby.*" "bartleby" @?= False

  -- '*' in the middle
  , testCase "a*c matches abc"   $ matchGlob "a*c" "abc" @?= True
  , testCase "a*c matches a.b.c" $ matchGlob "a*c" "a.b.c" @?= True
  , testCase "a*c matches ac (empty middle)" $
      matchGlob "a*c" "ac" @?= True
  , testCase "a*c does NOT match ab" $ matchGlob "a*c" "ab" @?= False

  -- Multiple '*'s
  , testCase "*foo* matches xfoo"  $ matchGlob "*foo*" "xfoo" @?= True
  , testCase "*foo* matches foox"  $ matchGlob "*foo*" "foox" @?= True
  , testCase "*foo* matches xfooy" $ matchGlob "*foo*" "xfooy" @?= True
  , testCase "*foo* matches foo"   $ matchGlob "*foo*" "foo" @?= True
  , testCase "*foo* does NOT match bar" $
      matchGlob "*foo*" "bar" @?= False

  -- '*' alone matches everything
  , testCase "* matches anything (non-empty)" $
      matchGlob "*" "anything" @?= True
  , testCase "* matches empty" $
      matchGlob "*" "" @?= True

  -- Case sensitivity
  , testCase "case-sensitive: *.BCARD does NOT match foo.bcard" $
      matchGlob "*.BCARD" "foo.bcard" @?= False
  ]
