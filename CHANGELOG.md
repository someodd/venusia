# Changelog for `Venusia`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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