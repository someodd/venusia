# O Venezia, Venaga, Venusia

**Venusia** is a fast and flexible Haskell library and daemon for building servers for the [Internet Gopher Protocol](https://en.wikipedia.org/wiki/Gopher_\(protocol\)). It's designed for ease of use, powerful configuration, and extensibility.

It works well with other tools in its ecosystem:

  * **Build:** Use [Bore](https://github.com/someodd/bore) for static site generation (phlogs, templates) for your gopherspace.
  * **Rank:** Use [RYVM](https://github.com/someodd/ryvm) to rank files for search results.

## Features

Venusia is a complete framework for creating modern Gopherholes.

#### As a Library

  * **Declarative Routing:** Easily define routes with support for wildcards.
  * **Built-in Handlers:** Ready-to-use handlers for directories and files.
  * **`.gophermap` Support:** Automatically serves `.gophermap` files for custom menus.
  * **Menu Builder:** An intuitive DSL for creating Gopher menus.

#### As a Daemon (`venusia-exe`)

  * **TOML Configuration:** Use a simple `routes.toml` to configure file servers and gateways.
  * **Hot Reloading:** Watches your directory and `routes.toml`, reloading automatically on changes with no downtime.
  * **Process Gateways:** Execute external commands (e.g., `cowsay`, `figlet`, `curl`) and serve their output.

## Getting Started

### Running the Daemon

The quickest way to start is with the `venusia-exe` daemon.

1.  **Create a `routes.toml` file:**

    This file configures your Gopher server's behavior.

    ```toml
    # routes.toml

    [[files]]
    selector = "/files/"
    path = "/home/user/gopherhole/"

    # Gateway to the 'cowsay' command.
    [[gateway]]
    selector = "/gateway/cowsay"
    command = "cowsay"
    arguments = ["$search"] # Takes a search query
    menu = true
    search = true
    wildcard = false
    ```

    TODO: You can also use wildcards--a neat feature, I need to show an example!

    I sugguest trying out using `.lhs` (Literate Haskell) scripts with something like:

    ```toml
    [[gateway]]
    selector = "/gateway/something"
    search   = false
    menu = false
    wildcard = false
    command  = "runghc"
    arguments = ["/var/gopher/output/example.lhs"]
    ```

    Note that for the above TOML config nothing is mapped to /, so to test try something like `gopher://localhost/1/files/`. Make sure to change the `path` in the example (for `[[files]]`) to a directory you wanna serve.

    You may also want to know in Gopher the root selector is actually blank `""` and not `"/"`.

2.  **Run the `watch` command:**

    Point the watcher to your directory, host, and port.

    ```bash
    venusia watch /path/to/your/gopherhole gopher.example.com 7070
    ```

    Your Gopher server is now live. Changes to your files or `routes.toml` will be reflected instantly.

### What about search support?

You can use [my ryvm software](https://github.com/someodd/ryvm) as a gateway like this, in your `routes.toml`:

```
[[gateway]]
selector = "/search"
search = true
wildcard = false
menu = false
command = "/var/gopher/source/search.sh"
arguments = ["$search"]
```

And here's the `/var/gopher/source/search.sh` (don't forget to `chmod +X`!):

```
#!/usr/bin/env bash
# search.sh <search> [HOST] [PORT]
s="$1"; h="${2:-gopher.someodd.zip}"; p="${3:-70}"
cd /var/gopher/output || exit 1

ryvm --ext-whitelist txt --make-relative . "$s" \
| awk -F'\t' -v h="$h" -v p="$p" '
function is_gophermap(path,   l,ok){
  ok=0
  if ((getline l < path) > 0) {
    sub(/\r$/,"",l)
    if (l ~ /^.{2,}\t[^\t]+\t[^\t]+\t[^\t]+$/) ok=1
  }
  close(path)
  return ok
}
{
  file=$1
  sel = ($2 && $2 != "") ? $2 : file
  score = $3
  snip = $4

  t = is_gophermap(file) ? "1" : "0"     # 1 = menu (gophermap), 0 = text
  printf "%s%s — %s [score %s]\t%s\t%s\t%s\r\n", t, sel, snip, score, file, h, p
}'
```

### Using the Library

To build a custom server, use Venusia in your own Haskell project.

1.  **Add the dependency** in `package.yaml`:

    ```yaml
    dependencies:
    - Venusia
    ```

2.  **Create your server:**

    ```haskell
    -- app/Main.hs
    module Main (main) where

    import Venusia.Server
    import Venusia.MenuBuilder
    import Venusia.FileHandler
    import Venusia.SearchHandler
    import qualified Data.Text as T

    import Control.Concurrent.MVar
    import Data.Maybe (fromMaybe)

    host :: T.Text
    host = "localhost"

    port :: Int
    port = 7070

    -- Define server routes
    routes :: [Route]
    routes =
      [ on "/hello" $ \_ ->
          return $ TextResponse "Hello, gopher!\r\n"

      , onWildcard "/echo/*" $ \req ->
          pure $ TextResponse $ fromMaybe "Nothing." (req.reqWildcard)

      , onWildcard "/files/*" $ \req ->
          case req.reqWildcard of
            Just path ->
              serveDirectory host port "/var/gopher" "/files/" path Nothing
            Nothing ->
              pure $ TextResponse "No path provided."
      ]

    -- Main entry point
    main :: IO ()
    main = do
      -- Wrap routes in MVar because serveHotReload requires mutable route list
      routesVar <- newMVar routes

      -- Start the new hot-reloadable, streaming-safe server
      serveHotReload (show port) noMatchHandler routesVar
    ```

## Response types

`Venusia.Server.Response` has four constructors. Pick the one whose memory model matches your payload:

| Use case | Constructor | Memory |
|---|---|---|
| Menus, errors, small generated text | `TextResponse` | Held in memory |
| Small in-memory binary blob | `BinaryResponse` | Held in memory |
| Static file on disk (any size) | `FileResponse` | Constant (32 KB chunks) |
| Generated, piped, or unbounded content | `StreamingResponse` | Constant; producer chooses pacing |

### Streaming responses

`StreamingResponse` takes a callback `(BS.ByteString -> IO ()) -> IO ()`. The producer is given a `send` action and runs to completion. Memory stays constant regardless of how much is emitted, the producer can use `bracket` to own its own resources, and a client disconnect surfaces as an exception from `send` that tears the producer down cleanly.

**Example: a counter that emits one line per second**

```haskell
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BS8

countingRoute :: Route
countingRoute = on "/count" $ \_ ->
  pure $ StreamingResponse $ \send ->
    mapM_ (\n -> send (BS8.pack ("line " <> show n <> "\r\n"))
                 >> threadDelay 1000000) [1 :: Int .. 10]
```

**Example: relay an upstream audio stream over Gopher**

This is the kind of thing the type was built for — proxy an Icecast/SHOUTcast MP3 stream as Gopher item type `9`. The `bracket` ensures the upstream connection is closed whether the listener disconnects, the upstream drops, or anything else throws:

```haskell
import Control.Exception (bracket)
import Network.Socket (close)
import Venusia.Server (streamFromHandle)
-- …open a TCP socket to the upstream and convert it to a Handle…

radioRoute :: Route
radioRoute = on "/radio" $ \_ ->
  pure $ StreamingResponse $ \send ->
    bracket openUpstream close $ \upHandle ->
      streamFromHandle upHandle send
```

Use `streamFromHandle` (exported from `Venusia.Server`) for the common case of streaming from any `Handle` in 32 KB pieces.

### Notes on long-lived streaming

A few things to know if you're wiring something like a radio relay:

* The server caps in-flight connections at 256 (see `maxConcurrentConnections` in `Venusia.Server`). If you expect more concurrent listeners, raise both that constant and the host's `ulimit -n`.
* Each accepted socket has Linux `TCP_USER_TIMEOUT` set to 120 s, so a stuck send fails after two minutes rather than pinning a thread + FD forever. This is the only line of defence against a slow-reading client; there is no read-side timeout once the response is in flight, since the producer's pacing is intentional.
* For hostile-network deployments, putting Venusia behind a reverse proxy that can enforce per-connection budgets is still recommended.

## Verification

### Property and integration tests

Run the full suite:

```bash
stack test
```

The suite uses [`tasty`](https://hackage.haskell.org/package/tasty) and is split into three groups:

* **`Venusia.Server`** — QuickCheck properties for `sanitizeSelector` (idempotent, no CR/LF in output, truncates at first line ending), `parseRequest` (selector / query split), and the `on` / `onWildcard` route matchers (exact match, wildcard capture, prefix/suffix shape).
* **`Venusia.MenuBuilder`** — QuickCheck properties for `item` (type char prefix, CRLF suffix, exactly three tabs), `menu` / `render` (terminator), and shape tests for `info` / `error'` / `gophermapRender`.
* **`integration`** — end-to-end tests bound to an ephemeral local socket. Each `Response` constructor is round-tripped through a real TCP connection (`TextResponse`, `BinaryResponse`, `FileResponse` with a multi-256KB file, `StreamingResponse` over an 8 MB generated body). RFC behaviours (embedded CRLF in the request line, empty selector, type-7 tab queries) are exercised. FD-leak resilience is checked by hammering a route whose streaming producer throws partway through, plus 200 sequential round-trips.

### LiquidHaskell refinements

The codebase has a small set of [LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell/) refinements at the *boundaries* — the inputs and outputs that interact with the outside world. They sit in `{-@ ... @-}` comments which GHC ignores; running `liquid` checks them.

Currently refined:

* `chunkSize :: {v:Int | v > 0}` (the streaming chunk size)
* `readTimeoutMicros :: {v:Int | v > 0}` (the slowloris guard)

Documented as extension points (not yet active until a `containsCRLF` measure is defined):

* `sanitizeSelector` postcondition — the output contains no CR or LF byte. The corresponding QuickCheck property runs on every `stack test`.

To verify locally:

```bash
# install the verifier (one-time; needs z3 on PATH)
cabal install liquidhaskell

# check a module
liquid -i src src/Venusia/Server.hs
```

## Debian Packages & `systemd`

For production, [Venusia offers Debian packages](https://github.com/someodd/venusia/releases) with `systemd` support to run as a managed daemon.

After installing a package, you can edit the service configuration at `/lib/systemd/system/venusia.service`. For example, to integrate with `bore`, you could change the `ExecStart` line to:

```
ExecStart=/usr/bin/venusia watch /var/gopher/source gopher.someodd.zip 7071 "/usr/bin/bore build --source /var/gopher/source --output /var/gopher/output" 10000000
```

Then, reload the `systemd` daemon and restart the service:

```
sudo systemctl daemon-reload
sudo systemctl restart venusia.service
```

## Configuration (`routes.toml`)

Configure the daemon by defining routes in `routes.toml`.

  * **`[[files]]`**: Serves static files.
      * `selector`: Gopher path prefix (e.g., `/files/`).
      * `path`: Local directory to serve.
  * **`[[gateway]]`**: Executes a shell command.
      * `selector`: Gopher path, can include a wildcard (`*`).
      * `command`: The command to run.
      * `arguments`: Command arguments. `$search` and `$wildcard` are replaced with user input.
      * `wildcard`: Set to `true` if the selector uses a wildcard.
      * `menu`: Set to `true` to format the command's output as a Gopher menu.
