# Changelog for `Venusia`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

* `StreamingResponse` constructor for constant-memory responses driven by a producer callback. Suitable for generated content, process pipes, TCP relays (e.g. proxying an internet-radio stream over Gopher), and anything else where the payload isn't a file on disk.
* `streamFromHandle` helper exported from `Venusia.Server` for streaming any `Handle` in 32 KB chunks.

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