{- | This is merely an example!

-}

module Main (main) where

import Venusia.Server
import Venusia.MenuBuilder
import qualified Data.Text as T

-- | Register your routes.
routes :: [Route]
routes =
  [ on "/hello" $ \_ ->
      return "Hello, gopher!\r\n"
  , onWildcard "/echo/*/something" $ \request ->
      case request.reqWildcard of
        Just wildcard -> pure wildcard
        Nothing -> pure "Nothing."
  , on "/search" handleSearch
  , onWildcard "/superSearch/*/bar" handleWildcardSearch
  ]

-- | Handler for the wildcard search route.
handleWildcardSearch :: Request -> IO T.Text
handleWildcardSearch request =
    case (request.reqWildcard, request.reqQuery) of
        (Nothing, _) ->
            pure $ info "Venusia was coded incorrectly, apparently."
        (_, Nothing) ->
            pure $ info "User error: missing query."
        (Just wildcard, Just query) -> do
            pure . info $ wildcard <> " AND " <> query

-- | Handler for search queries (Gopher item type 7).
handleSearch :: Request -> IO T.Text
handleSearch request = do
  let query =
        case request.reqQuery of
            Nothing -> ""
            (Just something) -> something
  -- Build the response
  return . render $
    [ info "Search results for: " <> query
    , text "Example file" "/fake" "localhost" 7070
    , directory "Example dir" "/fake" "localhost" 7070
    ]

main :: IO ()
main = serve "7070" routes