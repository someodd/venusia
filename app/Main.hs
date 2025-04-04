{- | This is merely an example!
-}
module Main (main) where

import Venusia.Server
import Venusia.MenuBuilder
import Venusia.FileHandler
import qualified Data.Text as T

host :: T.Text
host = "localhost"

port :: Int
port = 7070

-- | Register your routes.
routes :: [Route]
routes =
  [ on "/hello" $ \_ ->
      return $ TextResponse "Hello, gopher!\r\n"
  , onWildcard "/echo/*/something" $ \request ->
      case request.reqWildcard of
        Just wildcard -> pure $ TextResponse wildcard
        Nothing -> pure $ TextResponse "Nothing."
  , on "/search" handleSearch
  , onWildcard "/files/*" $ \request ->
      case request.reqWildcard of
        Just wildcard -> serveDirectory host port "/home/tilde" "/files/" wildcard
        Nothing -> pure $ TextResponse "No wildcard provided."
  , onWildcard "/superSearch/*/bar" handleWildcardSearch
  ]

-- | Handler for the wildcard search route.
handleWildcardSearch :: Request -> IO Response
handleWildcardSearch request =
  case (request.reqWildcard, request.reqQuery) of
    (Nothing, _) ->
      pure $ TextResponse $ info "Venusia was coded incorrectly, apparently."
    (_, Nothing) ->
      pure $ TextResponse $ info "User error: missing query."
    (Just wildcard, Just query) -> do
      pure . TextResponse . info $ wildcard <> " AND " <> query

-- | Handler for search queries (Gopher item type 7).
handleSearch :: Request -> IO Response
handleSearch request = do
  let query =
        case request.reqQuery of
          Nothing -> ""
          (Just something) -> something
  -- Build the response
  return . TextResponse . render $
    [ info "Search results for: " <> query
    , text "Example file" "/fake" host port
    , directory "Example dir" "/fake" host port
    ]

main :: IO ()
main = serve "7070" noMatchHandler routes