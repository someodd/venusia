# O Venezia, Venaga, Venusia

Venusia is a Haskell library for buildling [Internet Gopher Protocol](https://en.wikipedia.org/wiki/Gopher_(protocol)) servers and handling gophermaps/menus.

## WARNING: in alpha

This is currently experimental and the API will be constantly twisted to suit my needs (as a dependency) until the first release.

## Example

```haskell
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
      return $ textResponse "Hello, gopher!\r\n"
  , onWildcard "/echo/*/something" $ \request ->
      case request.reqWildcard of
        Just wildcard -> pure $ textResponse wildcard
        Nothing -> pure $ textResponse "Nothing."
  , on "/search" handleSearch
  , onWildcard "/files/*" $ \request ->
      case request.reqWildcard of
        Just wildcard -> serveDirectory host port "/home/tilde" "/files/" wildcard
        Nothing -> pure $ textResponse "No wildcard provided."
  , onWildcard "/superSearch/*/bar" handleWildcardSearch
  ]

-- | Handler for the wildcard search route.
handleWildcardSearch :: Request -> IO Response
handleWildcardSearch request =
  case (request.reqWildcard, request.reqQuery) of
    (Nothing, _) ->
      pure $ textResponse $ info "Venusia was coded incorrectly, apparently."
    (_, Nothing) ->
      pure $ textResponse $ info "User error: missing query."
    (Just wildcard, Just query) -> do
      pure . textResponse . info $ wildcard <> " AND " <> query

-- | Handler for search queries (Gopher item type 7).
handleSearch :: Request -> IO Response
handleSearch request = do
  let query =
        case request.reqQuery of
          Nothing -> ""
          (Just something) -> something
  -- Build the response
  return . textResponse . render $
    [ info "Search results for: " <> query
    , text "Example file" "/fake" host port
    , directory "Example dir" "/fake" host port
    ]

main :: IO ()
main = serve "7070" noMatchHandler routes
```

weird issue:

```
➜  gopherhole_bore curl "gopher://localhost:7070/1/files/../"     
3Not found: /			0
```

the `..` does this