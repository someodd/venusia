module Main (main) where

import Test.Tasty

import qualified Test.Venusia.Server as Server
import qualified Test.Venusia.MenuBuilder as MenuBuilder
import qualified Test.Venusia.Integration as Integration

main :: IO ()
main = defaultMain $ testGroup "Venusia"
  [ Server.tests
  , MenuBuilder.tests
  , Integration.tests
  ]
