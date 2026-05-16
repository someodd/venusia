# O Venezia, Venaga, Venusia

> *A modern gopher server in a single binary.
> Drop a directory, raise a gopherhole — quietly, slowly,
> while the rest of the web roars.*

[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![Latest release](https://img.shields.io/github/v/release/someodd/venusia)](https://github.com/someodd/venusia/releases/latest)
[![CI](https://github.com/someodd/venusia/actions/workflows/ci.yml/badge.svg)](https://github.com/someodd/venusia/actions/workflows/ci.yml)

**Live demo:** [`gopher://gopher.someodd.zip`](gopher://gopher.someodd.zip) — point your favourite gopher client at it. Or just `curl gopher://gopher.someodd.zip` from a shell.

---

## What it is

A self-contained gopher server. Point it at a directory; it serves the directory. Add a few lines of TOML and it runs subprocesses, streams long-lived processes, and executes files by extension. The same code is also a Haskell library.

Used in production at **gopher.someodd.zip**. Pairs with two ecosystem tools:

- **[Bartleby](https://github.com/someodd/bartleby)** — a scrivener for gopherspace: walks a library, reads sidecar `.bcard` metadata, writes `.gophermap` files and atom feeds under `catalog/`. Treat your gopherhole as a card catalog, not a website. (Currently MVP.)
- **[RYVM](https://github.com/someodd/ryvm)** — search ranking for type-7 selectors.

If you came here looking for a fast way to put a phlog online, you're in the right place. If you came here because you remember 1991 fondly, also yes.

## Quickstart

Everything below assumes a Debian-flavoured Linux. The project ships `.deb` packages that bundle the binary and a `systemd` unit.

```bash
# 1. Download the latest .deb from
#    https://github.com/someodd/venusia/releases/latest
sudo dpkg -i ~/Downloads/venusia_*.deb

# 2. Tell systemd which host and port to bind. The shipped unit doesn't
#    set them (every deployment differs); a drop-in override is the
#    simplest way:
sudo systemctl edit venusia.service
#  → in the editor that opens, paste, then save:
#
#    [Service]
#    ExecStart=
#    ExecStart=/usr/bin/venusia watch /var/gopher/source 127.0.0.1 7070
#
#  (empty ExecStart= clears the inherited default; the second sets the
#   new one. systemd convention.)

# 3. Tell Venusia to serve the directory, and drop in some content.
sudo tee /var/gopher/source/routes.toml > /dev/null <<'EOF'
[[files]]
selector = ""
path     = "/var/gopher/source"
EOF
echo "Hello from gopher!" | sudo tee /var/gopher/source/welcome.txt

# 4. Start
sudo systemctl restart venusia

# 5. See it
curl gopher://127.0.0.1:7070
```

Save a file in `/var/gopher/source/` → it shows up. That's the static-phlog story; everything else is opt-in TOML.

> Don't have a Debian box? Same flow with `stack build && stack exec -- Venusia-exe watch /path/to/dir 127.0.0.1 7070` instead of the `.deb` — the `routes.toml` from step 3 goes inside `/path/to/dir`. Requires [Stack](https://docs.haskellstack.org/).

## What can I do with this?

| If you want to… | Add… | Skip to |
|---|---|---|
| Serve a directory of files | `[[files]]` | [Configuration](#configuration) |
| Run `cowsay` (or anything) on demand | `[[gateway]]` | [Recipes](#recipes) |
| Auto-execute `.hs` / `.sh` / `.py` files | `[[script_extension]]` | [Recipes](#recipes) |
| Stream a long-running subprocess | `stream = true` | [Recipes](#recipes) |
| Type-7 search results | RYVM + a tiny shell gateway | [Recipes](#recipes) |
| Auto-rebuild on file change | watch hook + Bartleby | [Recipes](#recipes) |
| Build a custom server in Haskell | the `Venusia` library | [Library](#building-with-the-library) |
| Run it as a managed daemon | `.deb` + `systemd` | [Production](#production) |

## Configuration

The watcher looks for `routes.toml` in the watched directory.

Four top-level sections, each one a list of tables.

### `[[files]]` — serve a directory

```toml
[[files]]
selector = "/files/"        # gopher path prefix; "" is the root selector
path     = "/var/gopher/source"

  # Optional, nested: see [[files.script_extension]] below.
  # If present, files of that extension are executed instead of served as source.
```

A `[[files]]` block can carry any number of nested `[[files.script_extension]]` and `[[files.file_type]]` rules; both are described below.

### `[[gateway]]` — bind a selector to a process

```toml
[[gateway]]
selector      = "/cowsay"
command       = "/usr/games/cowsay"
arguments     = ["$search"]    # $search is the type-7 query, $wildcard the * match
wildcard      = false
as_info_lines = true           # wrap each output line as an info-line gophermap item
stream        = false          # set true for radio relays / large dumps / live tails
preamble      = []             # optional: literal gophermap lines before the output
postamble     = []             # optional: literal gophermap lines after the output
```

| Field | Meaning |
|---|---|
| `selector` | Gopher path. May contain a single `*` wildcard. |
| `command` / `arguments` | What to run. `$search` → request query. `$wildcard` → wildcard match. |
| `wildcard` | `true` if `selector` uses `*`. |
| `stream` | Pipe stdout via `StreamingResponse` (constant memory, child terminated on disconnect). |
| `as_info_lines` | Wrap each stdout line as `iLINE\t\t\t0\r\n`. Use when the gateway is reached via a menu-typed link. |
| `preamble` / `postamble` | Literal gophermap rows before/after the output. Auto-terminated with `\r\n` when `as_info_lines = true`. |

A search query is implied whenever `$search` appears in `arguments`; no separate `search` flag is needed.

### `[[files.script_extension]]` — run files of a given extension

**Nested under a `[[files]]` block.** Files inside that block's `path` whose extension matches one of these entries are executed by the configured runner; their stdout becomes the response. A `[[files]]` block with no nested `script_extension` rules never executes anything — the file is served as static content.

```toml
[[files]]
selector = "/cgi/"
path     = "/var/gopher/output/cgi/"

  [[files.script_extension]]
  extension     = "hs"           # without leading dot; case-insensitive
  command       = "runghc"
  arguments     = ["$file", "$selector", "$search", "$pathinfo"]
  stream        = true
  as_info_lines = false
```

| Placeholder | Resolved to |
|---|---|
| `$file` | Canonical absolute path to the script on disk. |
| `$selector` | Gopher selector that resolved to this script (e.g. `/cgi/figlet.hs`). Use it to emit menu items pointing back at the script without hardcoding its path. |
| `$search` | The request's query string (after the tab), or empty. |
| `$pathinfo` | Selector portion *after* the script filename, with a leading slash. A request for `/cgi/wiki.hs/Page/SubPage` runs `wiki.hs` with `$pathinfo = /Page/SubPage`; a request for `/cgi/wiki.hs/` gives `/`; `/cgi/wiki.hs` gives the empty string. Lets one script back a whole virtual sub-tree without one route per page. Modeled on CGI's `PATH_INFO`. |

The process's working directory is the file's parent directory, so `readFile "data.txt"` finds a sibling.

There is **no top-level `[[script_extension]]` table** — the rule lives where the executable does. This is deliberate (default-deny: a `[[files]]` block can't accidentally inherit script execution from a global pool).

### `[[file_type]]` — override directory-listing item types

Auto-generated directory listings emit a gopher item-type character per file (`0` text, `1` menu, `9` binary, `I` image, …). Both top-level and nested forms exist:

```toml
# Top-level: applies in every directory listing the daemon generates
[[file_type]]
extension = "md"
item_type = "0"

# Nested: scoped to one [[files]] block; wins over the top-level rule
# inside that block's listings only.
[[files]]
selector = "/cgi/"
path     = "/var/gopher/output/cgi/"

  [[files.file_type]]
  extension = "hs"
  item_type = "1"
```

Resolution order for the auto-generated listing:

1. **Nested `[[files.file_type]]`** on the serving `[[files]]` block, if defined for the extension.
2. **Top-level `[[file_type]]`**, if defined.
3. Otherwise, if a `[[files.script_extension]]` rule covers the extension: `'1'` when `as_info_lines = true`, else `'0'`.
4. Otherwise, the hardcoded fallback (`.txt → 0`, `.png → I`, `.html → h`, …).

User-authored `.gophermap` files always win — the gophermap author wrote the type character themselves; the server doesn't second-guess.

#### Why allow nesting on file_type but require it on script_extension?

`file_type` is cosmetic — a wrong rule shows the wrong icon. Globals are fine. `script_extension` is executive — a wrong rule executes code. Forcing executive rules into a `[[files]]` block makes it impossible to enable execution at-distance via an unrelated config edit.

## Recipes

### A library that auto-rebuilds (Venusia + Bartleby)

[Bartleby](https://github.com/someodd/bartleby) walks a directory of writings, reads sidecar `.bcard` metadata, and emits `.gophermap` files and atom feeds under `catalog/`. Venusia serves the directory; the change-hook re-runs Bartleby whenever a source file changes.

Library layout under `/var/gopher/library/`:

```
bartleby.conf
recipes/
  cheesecake.jpg
  cheesecake.jpg.bcard         # YAML sidecar; title, dates, description
  march-rain.txt
  march-rain.txt.bcard
poetry/
  …
catalog/                       # bartleby writes this in place
  .gophermap
  feed.xml
  recipes/.gophermap
  …
```

`bartleby.conf` (one per library, at the library root):

```yaml
hostname: gopher.example.com
port: 70
selector: /
```

Foreground (dev) — same arguments as the systemd `ExecStart` below, run directly. Ctrl-C to stop:

```bash
venusia watch /var/gopher/library gopher.example.com 70 \
  "/usr/bin/bartleby /var/gopher/library" \
  10000000
```

Or as a systemd override:

```ini
[Service]
ExecStart=
ExecStart=/usr/bin/venusia watch /var/gopher/library gopher.example.com 70 \
            "/usr/bin/bartleby /var/gopher/library" \
            10000000
```

The two trailing positional args (in both forms) are the change-hook command and a debounce delay in microseconds. Edit a source file under `/var/gopher/library/` and Bartleby rewrites the `catalog/` gophermap files in-place — Venusia keeps serving from the same directory, so the next request sees the new menu.

`routes.toml` (in `/var/gopher/library/`):

```toml
[[files]]
selector = ""
path     = "/var/gopher/library"
```

Curated entry point: `gopher://host/1/catalog/`. Raw directory browsing still works at `gopher://host/1/` for readers who want to ignore the catalog and rummage.

### Cowsay on demand

```toml
[[gateway]]
selector      = "/cowsay"
command       = "/usr/games/cowsay"
arguments     = ["$search"]
wildcard      = false
as_info_lines = true
```

Reachable as `gopher://host/7/cowsay` (item type 7, takes a query). The `as_info_lines` wraps the ASCII cow as info-line items so it renders inside a gopher menu.

### Auto-execute Haskell scripts

Drop scripts in a directory; they run on request.

```toml
[[files]]
selector = "/cgi/"
path     = "/var/gopher/scripts"

  [[files.script_extension]]
  extension     = "hs"
  command       = "runghc"
  arguments     = ["$file", "$selector", "$search"]
  stream        = true
  as_info_lines = false     # the script emits a real gophermap; don't 'i'-wrap

  [[files.file_type]]
  extension = "hs"
  item_type = "1"           # in directory listings, show .hs files as menu links
```

Now `/cgi/digest.hs` runs `runghc /var/gopher/scripts/digest.hs` and streams stdout. Sibling files (`runghc digest.hs` reading `data.txt` next to it) work because the working directory is the file's parent.

### Stream an internet radio relay

A `StreamingResponse` proxies bytes from an upstream socket without buffering. The simplest way is via a one-shot shell script:

```toml
[[gateway]]
selector  = "/radio"
command   = "/usr/local/bin/icestream.sh"
arguments = []
wildcard  = false
stream    = true
```

```bash
#!/bin/sh
# icestream.sh
exec curl -s --no-buffer https://stream.example.com:8000/main
```

Memory stays constant regardless of how long the listener stays connected; if they disconnect, Venusia sends `SIGTERM` (then `SIGKILL` after 2 s if necessary), so `curl` is reaped.

### Search results with RYVM

[RYVM](https://github.com/someodd/ryvm) ranks files; an `awk` postprocessor formats them as gopher-menu rows.

```toml
[[gateway]]
selector  = "/search"
command   = "/var/gopher/library/search.sh"
arguments = ["$search"]
wildcard  = false
```

```bash
#!/usr/bin/env bash
# /var/gopher/library/search.sh — chmod +x me
s="$1"; h="${2:-gopher.example.com}"; p="${3:-70}"
cd /var/gopher/library || exit 1

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
  t = is_gophermap(file) ? "1" : "0"
  printf "%s%s — %s [score %s]\t%s\t%s\t%s\r\n", t, sel, snip, score, file, h, p
}'
```

Reachable as `gopher://host/7/search`. The script outputs valid gophermap rows; no `as_info_lines` needed.

## Production

### Installing

`.deb` packages live on the [releases page](https://github.com/someodd/venusia/releases). Each ships:

- The `venusia` binary at `/usr/bin/venusia`.
- A `systemd` unit at `/lib/systemd/system/venusia.service`.
- Pre-install hooks that create a `venusia` system user and `/var/gopher/source/`.

### Configuring

The shipped unit does *not* set host or port — every deployment differs. Use `systemctl edit venusia.service` to add an override (see [Quickstart](#quickstart)).

For a Bartleby-integrated library, the override looks like:

```ini
[Service]
ExecStart=
ExecStart=/usr/bin/venusia watch /var/gopher/library gopher.example.com 70 \
            "/usr/bin/bartleby /var/gopher/library" \
            10000000
```

Then `sudo systemctl restart venusia`. (`systemctl edit` already reloaded the unit; no separate `daemon-reload` needed.)

**Drop-in override vs full edit.** `systemctl edit venusia.service` (what we used in the quickstart) creates a small drop-in file under `/etc/systemd/system/venusia.service.d/`. Package upgrades won't clobber it. If you'd rather edit the entire unit file (and accept that future `.deb` upgrades to the unit file won't merge into your version), use `sudo systemctl edit --full venusia.service` instead.

### Operating

- **Logs:** `journalctl -u venusia.service -f`.
- **Connection cap:** the accept loop is bounded by a `QSem` at 256 in-flight connections (see `maxConcurrentConnections` in `Venusia.Server`). Raise it *and* the host's `ulimit -n` together if you expect more.
- **Slow-reading client defence:** each accepted socket has Linux `TCP_USER_TIMEOUT` set to 120 s. Without this, a slow-reading client can pin a streaming response indefinitely.
- **Hostile networks:** putting Venusia behind a reverse proxy with per-connection budgets is recommended for public-internet exposure.

## Building with the library

The same code that powers the daemon is exposed as a Haskell library. Useful when you want behaviour the TOML doesn't cover — custom routing, dynamic content with full type safety, or embedding gopher in a larger service.

```haskell
-- app/Main.hs
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedRecordDot  #-}
module Main (main) where

import Venusia.Server
import Venusia.FileHandler
import qualified Data.Text as T
import Control.Concurrent.MVar (newMVar)
import Data.Maybe (fromMaybe)

host :: T.Text
host = "127.0.0.1"

port :: Int
port = 7070

routes :: [Route]
routes =
  [ on "/hello" $ \_ ->
      pure $ TextResponse "Hello, gopher!\r\n"

  , onWildcard "/echo/*" $ \req ->
      pure $ TextResponse (fromMaybe "Nothing." req.reqWildcard)

  , onWildcard "/files/*" $ \req ->
      case req.reqWildcard of
        Just sub -> serveDirectory host port "/var/gopher/source" "/files/" sub Nothing
        Nothing  -> pure $ TextResponse "No path provided."
  ]

main :: IO ()
main = do
  routesVar <- newMVar routes
  serveHotReload (show port) noMatchHandler routesVar
```

### Response types

`Venusia.Server.Response` has four constructors. Pick the one whose memory model matches your payload:

| Use case | Constructor | Memory |
|---|---|---|
| Menus, errors, small generated text | `TextResponse` | Held in memory |
| Small in-memory binary blob | `BinaryResponse` | Held in memory |
| Static file on disk (any size) | `FileResponse` | Constant (32 KB chunks) |
| Generated, piped, or unbounded content | `StreamingResponse` | Constant; producer chooses pacing |

`StreamingResponse` takes a callback `(BS.ByteString -> IO ()) -> IO ()`. The producer is given a `send` action and runs to completion. Memory stays constant regardless of how much is emitted, and the producer can use `bracket` to own its own resources. A client disconnect surfaces as an exception from `send` and tears the producer down cleanly — eventually; for a graceful FIN the kernel may continue accepting writes briefly, with `TCP_USER_TIMEOUT` (120 s) as the backstop. Example — relay an upstream MP3 stream as item type `9`:

```haskell
import Control.Exception (bracket)
import Network.Socket (close)
import Venusia.Server (streamFromHandle)

-- openUpstream is your code: open a TCP socket to the upstream server
-- and convert it to a 'Handle'. (System.IO.hSetBinaryMode / Network.Socket
-- handle conversion, or Network.Connection, or whatever you prefer.)

radioRoute :: Route
radioRoute = on "/radio" $ \_ ->
  pure $ StreamingResponse $ \send ->
    bracket openUpstream close $ \upHandle ->
      streamFromHandle upHandle send
```

`streamFromHandle` (exported from `Venusia.Server`) is the common case for streaming any `Handle` in 32 KB pieces.

### TOML-driven server, Haskell extras

Most TOML primitives are also library-exported, so a hybrid is straightforward:

- `Venusia.Routes.runProcess` — run a subprocess as a `Response`. Two flags pick the cell of a 2×2 matrix: `stream` (buffered vs piped) × `as_info_lines` (raw vs info-line-wrapped).
- `Venusia.Routes.mkScriptHook` — a file-extension hook, the same one `[[script_extension]]` uses internally.
- `Venusia.FileHandler.serveDirectoryWith` — `serveDirectory` with a per-file hook (`FilePath -> IO (Maybe Response)`) and a per-extension item-type override fn. Use this if you want the script-extension behaviour from Haskell without TOML.

## Internals

For contributors, or for the curious.

### Tests

```bash
stack test
```

68 tests, three groups:

- **`Venusia.Server`** — QuickCheck properties for `sanitizeSelector`, `parseRequest`, and the `on` / `onWildcard` matchers.
- **`Venusia.MenuBuilder`** — properties for `item`, `menu` / `render` (terminator), and shape tests for `info` / `error'` / `gophermapRender`.
- **`integration`** — end-to-end tests against a real local socket: each `Response` constructor round-tripped (8 MB streaming body, 256 KB file), RFC behaviours (CRLF in request, type-7 tab queries, empty selector), FD-leak resilience, the `runProcess` 2×2, the file-server hook, the directory-traversal guard, disconnect-kills-child, and the substitution contract.

### LiquidHaskell refinements

A small set of [LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell/) refinements live as comments on boundary constants — values that interact with the kernel or socket layer. They're inert to GHC; running `liquid` checks them.

Currently refined:

- `chunkSize :: {v:Int | v > 0}` (streaming chunk size)
- `readTimeoutMicros :: {v:Int | v > 0}` (slowloris guard)
- `maxConcurrentConnections :: {v:Int | v > 0}` (connection cap)
- `connectionWriteTimeoutMillis :: {v:Int | v > 0}` (write-side timeout)
- `cleanupGracePeriod :: {v:Int | v > 0}` (SIGTERM grace before SIGKILL)

Documented as an extension point (waiting on a `containsCRLF` measure):

- `sanitizeSelector` postcondition — the output contains no CR or LF byte. The corresponding QuickCheck property runs on every `stack test`.

To verify locally:

```bash
cabal install liquidhaskell        # one-time; needs z3 on PATH
liquid -i src src/Venusia/Server.hs
```

### Hardening

- Concurrent-connection cap (256) bounds FD usage under floods.
- `TCP_USER_TIMEOUT` on accepted sockets reaps stuck writes.
- Streaming children are reaped via `bracket`: SIGTERM, 2 s grace, SIGKILL.
- Streaming children's stdin is closed (`NoStream`); they cannot read the daemon's stdin.
- Selectors are sanitised at the first CR/LF (RFC 1436); embedded line endings cannot smuggle a second request.
- Directory traversal is checked on path components, not raw strings (no `/var/gopher` masquerading as an ancestor of `/var/gopher2/...`).

## Changelog

See [CHANGELOG.md](CHANGELOG.md). The project follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/) and [Keep a Changelog](https://keepachangelog.com/).

## Contributing

Issues and pull requests welcome at <https://github.com/someodd/venusia>. The `master` branch is what runs at gopher.someodd.zip; CI on every push must be green for merges. New features should come with tests in `test/Test/Venusia/`.

## License

BSD-3-Clause. See [LICENSE](LICENSE).

---

*A protocol older than the web, quieter than the web.
A server that intends to be small forever.*
