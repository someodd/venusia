# Changelog for `Venusia`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.11.4.0 - 2026-05-23

### Fixed

* **Connections now close with a clean TCP FIN instead of an abortive RST.** After writing a response the server called `close` on the client socket immediately. When a socket is closed while unread bytes remain in its kernel receive buffer, Linux aborts the connection with an RST rather than a FIN — so clients saw `connection reset by peer` (curl exit code 56) intermittently, even though the full response body had already arrived. A real client sends `selector\r\n`, and whenever the trailing bytes landed in a separate TCP segment that arrived after the server's single `recv` returned, those bytes sat unread at close time and triggered the abort; the segment-timing dependence is what made it intermittent and made it hit every route (static files, scripts, streaming alike). The connection handler now tears down through `gracefulClose`, which sends FIN, drains any unread inbound bytes, then closes — so the handshake completes cleanly and client exit codes are no longer trashed. A new `gracefulCloseTimeoutMillis` (2 s) caps how long the drain waits for the peer's FIN. Covered by a regression test that deliberately leaves unread bytes in the receive buffer and asserts a clean EOF.

## 0.11.3.0 - 2026-05-22

### Changed

* **The shipped `systemd` unit now listens on port 70 by default.** The packaged `ExecStart` changed from `… 127.0.0.1 7070` to `… gopher.example.com 70`, and the unit gained `AmbientCapabilities=CAP_NET_BIND_SERVICE` plus a matching `CapabilityBoundingSet=CAP_NET_BIND_SERVICE`. The unprivileged `venusia` user can now bind the privileged gopher port directly — the process still never runs as root, and the bounding set is restricted so port-70 binding is the only privilege the service can ever hold. Operators no longer need a reverse proxy or port rewrite just to serve `:70` natively. The HOST arg (`gopher.example.com`) is only a placeholder printed in generated menu links — it does not affect which interface is bound — so the only deployment-specific override now needed is the real hostname (recommended via `systemctl edit --full venusia.service`, which opens the whole unit for an in-place `ExecStart` edit). README quickstart and Production sections updated to match.

## 0.11.2.0 - 2026-05-18

### Fixed

* **README.gophermap preview header link is now type 1 (menu), not type 0 (text).** Venusia hardcodes the recognition of `README.gophermap` as a menu source — when it appears at the top of an auto-listing, the file's contents are rendered inline through `gophermapItems`. The link to the source file was bypassing the per-extension item-type resolver entirely (it used the hardcoded `text` helper, which is `item '0'`), so even operators who configured `[[files.file_type]] extension = "gophermap" item_type = "1"` saw `0README.gophermap (…)` in their listings. The header now picks its item-type by which README mode is active: `'1'` for `.gophermap`, `'0'` for `.txt`. No config detour — Venusia's own recognition of the file as a menu is the authority.

### Changed

* **`.gophermap` files are rendered through `gophermapRender` on direct fetch**, not served as raw bytes. Mirrors the existing behaviour for a directory's index `.gophermap`. So a type-1 link to a `.gophermap` file now delivers a real parsed menu to the client (host/port filled in for bucktooth two-field shorthand), instead of raw shorthand that clients parse inconsistently or render malformedly.

* **Default item-type for `.gophermap` in `fileExtensionToItemType` is now `'1'` (menu)**, not the unknown-extension fallback `'9'` (binary download). Mirrors how `.html → 'h'` works — the extension marks the file as its own format, not as something to download. Operators who want different behaviour can still override per-block via `[[files.file_type]]`.

## 0.11.1.0 - 2026-05-17

### Fixed

* **`stdout` is now line-buffered, not block-buffered.** Haskell's default `stdout` buffering is block-buffered when the handle isn't a TTY — including the case where `systemd-journald` collects output via a pipe. That made `putStrLn` invisible in `journalctl` until enough bytes accumulated to flush a block (4–8 KB), so a low-volume daemon could appear completely silent for hours. The first thing `main` now does is force line-buffering on both `stdout` and `stderr`, so log lines land in `journalctl` as they happen. (This was the most visible piece of "the watcher is broken" symptoms on a busy production host.)

* **File watcher no longer deadlocks when the hook fails.** Previously, the watcher acquired an MVar lock on each event, ran `callCommand` (which throws on non-zero exit), then released the lock. If the hook ever exited non-zero, the exception propagated out before the lock was released → the MVar stayed empty forever → every subsequent file event was silently dropped. Now the forked handler is wrapped in `finally` so the lock is always returned, and both the hook and the subsequent `reloadRoutes` are wrapped in `try` so each failure is logged as `Hook FAILED …` / `Reload FAILED …` and the watcher stays alive for the next change.

* **Forked watcher thread death is surfaced.** `app/Main.hs` ran `forkIO $ watchForChanges …` without an exception handler. If `withManager` or `watchTree` from fsnotify threw (inotify init failure, unsupported filesystem, watch directory missing), the forked thread died, the main thread happily continued into `serveHotReload`, and hot-reload was silently gone with no log signal. The forked body is now wrapped in `catch`, so a watcher death logs `WATCHER THREAD DIED: <reason>` to `journalctl`.

### Changed

* **`venusia watch` registers the file watch regardless of whether a `CHANGE_HOOK` is provided.** Previously, omitting the hook command made the `watch` subcommand do nothing watch-related at all — the daemon served fine, but `routes.toml` edits required a full restart to take effect. The watch is now always active; the hook, if any, is just an optional pre-reload step inside the change handler.

* **Watcher emits a `Watch registered on <dir>` line after `watchTree` returns**, and a `fsnotify event: …` line for every raw event. Lets operators tell, from `journalctl` alone, whether the watch is alive and whether events are arriving at all — distinguishing "fsnotify silently failed" from "watch active but no events" from "events arriving, handler suppressed by lock."

### Docs

* New **Troubleshooting the watcher** subsection in the production guide, walking through the four most common causes of "the hook isn't running": dead watcher thread, exhausted inotify limit, filesystem that doesn't deliver inotify events, and hook-failure deadlock (now fixed but still worth logging).

## 0.11.0.0 - 2026-05-16

### Added

* **`unlisted` field on `[[files]]` blocks.** Filename glob patterns whose matches are hidden from the auto-generated directory listing. **Listings only — direct fetches by exact selector still return the file.** Use it to tidy operator-facing files (Bartleby's `bartleby.conf`, sidecar `*.bcard`, atom `feed.xml`) out of the raw menu surface without breaking hand-written gophermap links or tools that read those files.

  ```toml
  [[files]]
  selector  = ""
  path      = "/var/gopher/library"
  unlisted  = ["bartleby.conf", "*.bcard"]
  ```

  Glob syntax: `*` matches any run of characters (including the empty string); everything else is literal; per-filename, per-directory; case-sensitive. No `?` or character classes.

* **`Venusia.FileHandler.matchGlob`** is exported alongside the new `unlisted` plumbing, so library callers that want the same "this filename matches that glob" semantic can use it directly.

### Changed

* **Breaking: `serveDirectoryWith` and `listDirectoryAsGophermapWith` gain a final `[T.Text]` positional argument** for the `unlisted` glob patterns. Direct callers need to append `[]` to preserve current behaviour. The TOML-driven path is unaffected. The simple `serveDirectory` wrapper's signature is unchanged.

## 0.10.1.0 - 2026-05-16

### Fixed

* **`[[files]]` mount selectors are matched on path-segment boundaries.** A `selector = "/applets"` route used to match any request whose path *started* with `/applets`, so `/applets.bcard` would slip into the `/applets` mount and be resolved inside its served root as the relative path `.bcard` — a dotfile, refused with a misleading "dotfile paths are disabled" error. `onWildcard` now requires the prefix to end at a `/`, consume the whole selector, or be followed by a `/` (wildcard patterns containing `*` keep substring semantics).

### Added

* **`README.gophermap` is rendered as a directory-listing preview**, alongside the existing `README.txt` behaviour. When both files are present, `README.gophermap` wins so the preview can carry real gopher items (sub-menus, text links, search) instead of info-only prose. Bucktooth-style shorthand is supported — a two-field line (`0Display\tselector`) fills in the server's host/port automatically, identical to how `.gophermap` indexes work.

### Changed

* **`Venusia.MenuBuilder` now exports `gophermapItems`**, the parser that backs `gophermapRender` returned as a `[T.Text]` list of items without the terminator. Use this when you want to splice gophermap content into a larger menu (e.g. an embedded preview) rather than emit a standalone gophermap. `gophermapRender` is now `menu . gophermapItems` and produces the same bytes as before.

### Docs

* **Bartleby auto-rebuild recipe shows a foreground `venusia watch …` invocation** alongside the existing systemd `ExecStart` form, for local development without packaging a unit.

## 0.10.0.1 - 2026-05-11

### Fixed

* **Parent-directory link present at every block root with a non-empty selector.** Previously the auto-generated listing suppressed the "Parent directory (..)" row whenever `makeRelative` returned `"."` — i.e. at the root of every `[[files]]` block — which left users with no in-UI path back to the broader gopher tree from places like `/applets`. The row is now emitted at block root pointing one level up in the gopher namespace (so `/applets` → `/`, `/foo/bar` → `/foo`); only the catch-all root (empty selector + relativePath `"."`) correctly omits it.

### Changed

* **`README.txt` preview label includes the modification date** alongside the size, matching the columns used in the rest of the listing. Format: `README.txt (1.0KB, 2026-05-24):`.

## 0.10.0.0 - 2026-05-11

### Added

* **`$remote_ip` substitution token for `[[files.script_extension]]`.** Scripts can now opt into receiving the connecting client's IP address as text (IPv4 dotted-quad or IPv6 colon form) by including `$remote_ip` in the `arguments` template — useful for per-IP rate limiting, ban lists, audit logging, and other policy decisions that need to know who's calling. The value is the empty string when the peer can't be looked up (e.g. unix-socket connections, `getPeerName` failures). The accept loop captures the address once via `getNameInfo` with `NI_NUMERICHOST` and stamps it onto every `Request`; route matchers don't see it. Privacy note: Venusia just plumbs the value through, the script decides what to do with it.

### Changed

* **`Request` record gains `reqClientIp :: T.Text`.** Breaking for direct library users who pattern-match positionally or construct `Request` values. Route matchers (`on` / `onWildcard`) set this to `""` at match time; `dispatch` stamps the real address in before invoking the handler.

* **`mkScriptHook` now takes a client-IP argument** (fifth positional, before the file path) and `substituteScriptArg` gains a sixth argument for the same. Breaking for direct library users; the only documented caller is `createFileHandler`, which has been updated.

* **`dispatch` signature changed** to take the client IP as a fourth argument (before the request line text). Routes' `runOnSocket` integration handles this internally; only direct callers of `dispatch` need to update.

### Fixed

* **`"Directory listing for: ."` now shows the block's selector.** At the root of a `[[files]]` block, `makeRelative` returns `.` (the requested path equals the served root), and the listing header used to display that literal dot. Now shows the block's configured `selector` (e.g. `"Directory listing for: /applets"`), or `"/"` for the catch-all empty-selector block.

* **`README.txt` is no longer listed twice in auto-generated directory listings.** When `README.txt` is present, its content is rendered as a preview at the top of the listing; previously it also appeared in the regular file table below. Filtered out of the file table when it's the preview source.

* **`README.txt` preview label includes the file's size.** Was `"README.txt:"`; now `"README.txt (4.2KB):"` using the existing `formatFileSize` helper.

## 0.9.0.1 - 2026-05-11

### Fixed

* **.deb pre-install no longer re-asserts ownership/mode on an existing `/var/gopher`.** Previous releases ran `install -d -o venusia -g venusia -m 0770 /var/gopher` unconditionally, which on upgrade silently flipped a manually-created `tilde:tilde 0755` directory to `venusia:venusia 0770` — breaking SFTP / non-deb access patterns admins had set up. The pre-install now only creates `/var/gopher` (with the daemon-friendly ownership/mode) when the directory doesn't already exist. If the admin pre-created `/var/gopher` with their own layout, the .deb leaves it alone.

## 0.9.0.0 - 2026-05-11

### Security

* **Dotfile paths refused by default.** The file server now refuses any direct request whose path contains a Unix-style dotfile component (e.g. `/foo/.env`, `/.git/config`) and hides dotfiles from auto-generated directory listings. The refusal happens before file resolution, so an attacker who knows or guesses an exact filename can't fetch `.env`, `.git/config`, `.htpasswd`, SSH config, gvfs scratch files, etc. Set `allow_dotfiles = true` on a specific `[[files]]` block to opt back in for that block (needed only when the served content really is dotfiles, e.g. a dotfiles-as-content repo). The block's configured `index_file` (default `.gophermap`) is always exempt — that one dotfile name is the framework's directory-menu source by convention, so a direct request for it returns the menu's source text rather than being denied.

### Changed

* **Default watch directory is now `/var/gopher` (was `/var/gopher/source`).** The .deb pre-install creates `/var/gopher` directly with `venusia:venusia` ownership and the systemd unit watches it; routes.toml lives at `/var/gopher/routes.toml`. Existing installs that relied on the old `/var/gopher/source` layout will need an override at `/etc/systemd/system/venusia.service.d/override.conf` (or move their files up one level) — see the in-unit comment for the override snippet.

* **`serveDirectoryWith` gained two trailing arguments** for `allow_dotfiles` (`Bool`) and the directory-menu filename (`FilePath`, default `.gophermap`). Breaking for direct library users — the only documented caller is `createFileHandler`, which has been updated. Pass `False ".gophermap"` to preserve the default policy.

* **Directory-menu filename is now configurable per `[[files]]` block** via the new `index_file` TOML key (default `.gophermap`). Whatever it's set to is both the file Venusia reads to render a directory's menu and the single dotfile name exempt from the new dotfile refusal. Set it to a non-dotfile (e.g. `index.gph`) if you'd rather your menu sources not start with `.`.

* **File-server selectors are now clean absolute paths.** Auto-generated directory listings at the served root previously emitted entries like `./catalog` (because `makeRelative` returns `.` for "same directory" and the old joining code passed it through), and entries under a catch-all `selector = ""` block lacked the leading `/`. Both are now normalized: every selector in a listing is absolute and free of `.` artifacts.

### Added

* **Demo `routes.toml` seeded by the .deb on first install.** Fresh installs now drop a commented `routes.toml` into `/var/gopher/` on `apt install`, so the server has something to load besides "no routes loaded" out of the box. The demo includes a `[[gateway]]` for `/hello`, a catch-all `[[files]]` block, and a working `[[files.script_extension]]` for `.sh` files (with a commented-out `.lhs` block ready to uncomment once GHC is on the host). All four substitution tokens — `$file`, `$selector`, `$search`, `$pathinfo` — are wired up and documented inline. The default lives at `/usr/share/venusia/default-routes.toml` (also bundled by the package); post-install only seeds it when no `/var/gopher/routes.toml` exists yet, so upgrades leave your edits alone.

## 0.8.0.0 - 2026-05-09

### Added

* **`$pathinfo` substitution for `[[files.script_extension]]`.** A request for `/cgi/wiki.lhs/Page/SubPage` now executes `wiki.lhs` (instead of 404'ing) and exposes `/Page/SubPage` to the script via the new `$pathinfo` placeholder. A trailing slash with nothing after it (`/cgi/wiki.lhs/`) yields `/`; a request that addresses the script directly (`/cgi/wiki.lhs`) yields the empty string. The split happens before disk resolution: the script-prefix is canonicalised and bounds-checked exactly as today, and the path-info bytes never participate in file lookup — they reach only the script, the same way `$search` does. Lets a single script back a whole virtual sub-tree (wikis, hierarchical browsers) without declaring one `[[files]]` block per page. Modeled on CGI's `PATH_INFO`.

### Changed

* **`mkScriptHook` now takes a path-info argument** (breaking for direct library users — but the only documented call site is via `[[files.script_extension]]`, which has been updated). Pass `""` to opt out and preserve previous behaviour.

## 0.7.1.0 - 2026-05-07

### Fixed

* **Trailing-slash equivalence in `onWildcard`.** A `[[files]]` block with `selector = "/applets/"` now matches both `/applets/` and `/applets`. Previously the version without the trailing slash 404'd, which surprised clients that don't normalise trailing slashes consistently. Bare-prefix patterns (e.g. `/foo` without trailing slash) and wildcard patterns (e.g. `/files/*`) are unaffected. The `Request.reqSelector` passed to the handler is preserved as the request actually arrived; only matching is relaxed.

## 0.7.0.0 - 2026-05-07

This release rebases `[[script_extension]]` onto `[[files]]` blocks: the rule for *what to do with an extension* now lives next to the directory that owns it, not in a global pool. Same for `[[file_type]]` (additive — top-level still works for genuinely global rules).

### Breaking

* **`[[script_extension]]` is no longer a top-level table**. Move each rule inside the `[[files]]` block it should apply to, as `[[files.script_extension]]`. There is no global script-extension pool any more — a `[[files]]` block executes scripts iff it has a matching `[[files.script_extension]]` entry. Default-deny by construction.
* **`run_scripts` field on `[[files]]` is removed**. Activation is now implicit via the presence of `[[files.script_extension]]`.

  Migration:
  ```toml
  # Before (0.6.0.0)
  [[script_extension]]
  extension = "lhs"
  command = "stack"
  arguments = ["script", "$file", "--", "$search"]

  [[files]]
  selector = "/cgi/"
  path = "/var/gopher/output/cgi/"
  run_scripts = true

  # After (0.7.0.0)
  [[files]]
  selector = "/cgi/"
  path = "/var/gopher/output/cgi/"

    [[files.script_extension]]
    extension = "lhs"
    command = "stack"
    arguments = ["script", "$file", "--", "$search"]
  ```

### Added

* **Nested `[[files.file_type]]`** — per-block item-type overrides. Wins over top-level `[[file_type]]` rules within that `[[files]]` block's directory listings. Top-level `[[file_type]]` still works for genuinely global rules (e.g. `.md = "0"` everywhere).
* **`$selector` substitution** in `[[files.script_extension]]` arguments. Resolved to the gopher selector that matched the request (e.g. `/cgi/figlet.lhs`). Lets a script generate menu items pointing back at itself without hardcoding its own path — useful when a script's selector can change (different `[[files]]` mount, dir rename).
* `FilesConfig`, `RoutesConfig`, and `routesConfigCodec` are now exported from `Venusia.Routes` so library users (and the test suite) can construct or decode configs directly.

### Why the asymmetry between file_type and script_extension

`file_type` is cosmetic — a wrong rule shows the wrong icon in a directory listing. `script_extension` is executive — a wrong rule executes code. Globals are fine for cosmetic rules; they're a footgun for executive ones (a global `[[script_extension]]` plus an unrelated `[[files]]` block opting in via the old `run_scripts` flag was an at-distance way to enable execution somewhere you didn't intend). Forcing executive rules to live inside the block they apply to closes that.

### Tests

* New: tomland decodes `[[files.script_extension]]` and `[[files.file_type]]` nested arrays correctly (`test_tomlandNestedArrayDecode`).
* New: `$selector` substitutes with the request's selector (`test_scriptExtensionSelectorSubstitution`).
* Repurposed: `test_scriptExtensionTokenPassthrough` now uses `$wildcard` (still unsubstituted) instead of `$selector` (now recognised).

## 0.6.0.0 - 2026-05-07

### Added

* `StreamingResponse` constructor for constant-memory responses driven by a producer callback. Suitable for generated content, process pipes, TCP relays (e.g. proxying an internet-radio stream over Gopher), and anything else where the payload isn't a file on disk.
* `streamFromHandle` helper exported from `Venusia.Server` for streaming any `Handle` in 32 KB chunks.
* `[[script_extension]]` config table — files whose extension matches a registered spec are executed by the configured runner (e.g. `runghc $file`) instead of served as source. Active per `[[files]]` root via the new `run_scripts = true` opt-in (defaults to `false` for safety). Argument substitution: `$file` is the canonical absolute file path, `$search` the request query.
* `[[file_type]]` config table — overrides the gopher item-type character used in auto-generated directory listings, per extension. Resolution order: `[[file_type]]` > `[[script_extension]]` default (`'1'` if `as_info_lines`, else `'0'`) > hardcoded `fileExtensionToItemType`.
* `stream :: Maybe Bool` field on `[[gateway]]` and `[[script_extension]]` — pipes process stdout via `StreamingResponse` instead of buffering. Constant memory; the child process is terminated if the client disconnects.
* `as_info_lines :: Maybe Bool` field on `[[gateway]]` and `[[script_extension]]` — replaces the misnamed `menu` flag. Wraps each output line as an info-line gophermap item (`iLINE\t\t\t\r\n`).
* `runProcess`, `mkScriptHook`, `resolveItemType`, `ScriptExtensionConfig`, `FileTypeConfig` exported from `Venusia.Routes` so library users can build streaming-process gateways and file-server hooks without going through the TOML loader.
* `Venusia.FileHandler.serveDirectoryWith` — a variant of `serveDirectory` that takes a per-file hook (`FilePath -> IO (Maybe Response)`, returning `Just` short-circuits the default `FileResponse`) and an item-type override fn. The existing `serveDirectory` is now a thin wrapper.

### Changed

* **Breaking:** `[[gateway]] menu` field is **removed**. Rename `menu = true|false` to `as_info_lines = true|false` in your `routes.toml`. No alias, no deprecation period — the field is simply not parsed.
* Process execution in gateways now flows through a single `runProcess` 2×2 dispatcher (buffered/streamed × raw/info-line-wrapped). The buffered cells preserve the exact pre-existing semantics — same output, same gopher terminator, same preamble/postamble handling — so once you've renamed `menu`, existing buffered behaviour is bit-for-bit unchanged.
* Preamble/postamble entries are now auto-terminated with `\r\n` when `as_info_lines = true`. Previously a postamble like `["7Search\t/search\thost\t70"]` (no trailing `\r\n`) glued onto the gopher terminator, producing a malformed final line. Raw-output mode is unchanged (binary preambles, if any, stay verbatim).
* Extension-registry lookups are now case-insensitive on both sides — `digest.LHS` matches `extension = "lhs"`, and `extension = "LHS"` in TOML still matches `digest.lhs`. The filesystem itself stays case-sensitive.

### Security / hardening

* **Directory-traversal guard fixed.** The pre-existing path-containment check used string-level `isPrefixOf`, so a served root of `/var/gopher` was treated as an ancestor of `/var/gopher2/secret`. With multiple `[[files]]` roots whose paths shared a string prefix, a request like `/files/../gopher2/secret` could canonicalise into the sibling and slip past the guard. Now compares `splitDirectories` of root and path so only true path ancestors match.
* **Streaming child process is reaped, bounded.** Cleanup now sends SIGTERM, waits up to 2 seconds, then SIGKILLs via `signalProcess`. Previously a child that ignored SIGTERM could hang `waitForProcess` indefinitely, pinning the connection thread.
* **Streaming child cannot read the daemon's stdin.** `procSpec` sets `std_in = NoStream`. The buffered path was already safe (it pipes empty input via `readCreateProcess`), but the streaming path was inheriting stdin from the parent.
* **Streaming info-wrap is UTF-8 safe.** The read handle now has `hSetEncoding utf8` applied explicitly, so a non-UTF-8 daemon locale doesn't crash mid-stream when a script emits non-ASCII output.
* **CRLF in script output no longer corrupts info-line items.** `streamProcessInfoWrap` strips a trailing `\r` before wrapping, so a Windows-line-ending script doesn't produce `iLINE\r\t\t\t0\r\n`.

### Changed

* `FileResponse` now flows through the same streaming path as `StreamingResponse`. No behavior change — still 32 KB chunks, still constant memory.
* The connection socket is now closed via `finally`, so a thrown handler (or a streaming producer that hits a broken pipe) cannot leak the file descriptor.
* The initial request `recv` now has a 30 s timeout. Defends against slowloris-style holders that open a connection and never speak.
* Selectors with embedded CR/LF are now truncated at the first line ending (per RFC 1436), which closes a small log-injection / response-smuggling vector.
* `TCP_NODELAY` is set on the listening socket so small responses (menus, errors) don't sit in Nagle's buffer.
* `Venusia.Server` now exposes `runOnSocket` (run the accept loop on an already-bound socket). `parseRequest` and `sanitizeSelector` are also exported under an "Internal (exported for testing)" haddock section.
* `Request` now derives `Eq`.

### Security / hardening

* **Concurrent-connection cap.** The accept loop is now bounded by a `QSem` (default 256). Once that many handlers are in flight, `accept` blocks until one finishes. Defends against connection floods that would otherwise exhaust the server's FD limit (default `ulimit -n` is 1024).
* **Write-side timeout.** Each accepted socket gets `TCP_USER_TIMEOUT = 120 s` (Linux only; silently no-op elsewhere). Without this a slow-reading client could pin a streaming response forever — `sendAll` blocks once the kernel send buffer fills, with no read-side timeout to save us. With this, the kernel reaps the connection if no ACK arrives within two minutes.
* **Handler exception logging.** Synchronous exceptions thrown by handlers or streaming producers are now caught in `handleConnHotReload`, logged to stderr as a single line via `displayException`, and swallowed. Previously they propagated to GHC's default uncaught-exception handler and dumped multi-line `CallStack` traces. Async exceptions (`SomeAsyncException`, e.g. `killThread`) are re-thrown so cooperative shutdown still works.

### Tests

* Real test suite using `tasty` + `tasty-quickcheck` + `tasty-hunit`. Property tests for `sanitizeSelector`, `parseRequest`, the route matchers, and `MenuBuilder`. Integration tests round-trip every `Response` constructor through a real local socket (including an 8 MB streaming body and a 256 KB file), exercise embedded-CRLF defence, type-7 tab queries, and verify FD safety under producer exceptions and 200 sequential connections.

### LiquidHaskell

* Refined `chunkSize` and `readTimeoutMicros` as positive at the type level (`{-@ … :: {v:Int | v > 0} @-}`). The `sanitizeSelector` postcondition (no CR/LF in output) is documented as the next extension point and is currently enforced by a QuickCheck property.

## 0.5.0.0 - 2025-12-09

### Changed

* Space-efficient streaming of files so the GHC heap doesn't balloon after sending large files! This is what the filehandler now uses.

### Removed

* Old `serve` function!
* Examples

## 0.4.0.0 - 2025-09-30

### Added

* You can now use $wildcard and $search in the preamble and postamble

## 0.3.0.0 - 2025-09-01

### Removed

* Built-in search. However, I included an example of implementing search as a `[[gateway]]` using a script that uses `ryvm`.

## 0.2.0.0 - 2025-08-28

### Added

* Search+files support in gateway/routes config
* Host/port config from watch cli

## 0.1.0.0 - 2025-08-05

### Removed

* All things related to systemd in the software itself, I'm just going to let the workflow manage that.