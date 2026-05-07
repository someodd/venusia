{- | Gopher Route Configuration

This module handles parsing a routes.toml file to configure and build all
server routes, including command gateways, file servers, and search handlers.
-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE DataKinds             #-}

module Venusia.Routes
  ( loadRoutes
  , runProcess
  , resolveItemType
  , mkScriptHook
  , ScriptExtensionConfig (..)
  , FileTypeConfig (..)
  ) where

import           Control.Exception          (SomeException, bracket, catch, try)
import qualified Data.ByteString            as BS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Maybe                 (fromMaybe)
import qualified System.Process             as P
import           System.IO                  (Handle, hIsEOF,
                                             hSetBinaryMode, hSetEncoding,
                                             utf8)
import           System.IO.Error            (catchIOError)
import qualified System.Posix.Signals       as Posix
import           System.Timeout             (timeout)
import           GHC.Generics               (Generic)
import           GHC.Records                (HasField (getField))
import           Control.Monad              (forM_, unless)


import qualified Toml
import           Toml                       (TomlCodec, (.=))

import           System.FilePath            (takeDirectory, takeExtension)

import           Venusia.MenuBuilder        (error', info, render)
import           Venusia.Server             (Handler, Request (..), Response (..),
                                             Route, on, onWildcard, streamFromHandle)
import           Venusia.FileHandler        (fileExtensionToItemType, serveDirectoryWith)

-- Configuration Data Types ---

-- | Top-level configuration holding lists for each route type.
data RoutesConfig = RoutesConfig
  { gateways         :: [GatewayConfig]
  -- ^ Command gateway configurations.
  , files            :: [FilesConfig]
  -- ^ File server configurations.
  , scriptExtensions :: [ScriptExtensionConfig]
  -- ^ Per-extension script-runner specifications. Active under any
  -- @[[files]]@ root that opts in via @run_scripts = true@.
  , fileTypes        :: [FileTypeConfig]
  -- ^ Per-extension overrides for the gopher item-type character used in
  -- auto-generated directory listings. Highest priority in the resolver.
  } deriving (Show, Eq, Generic)

-- | Configuration for a command-based gateway process.
data GatewayConfig = GatewayConfig
  { selector    :: T.Text
  , command     :: T.Text
  , arguments   :: [T.Text]
  , wildcard    :: Bool
  , asInfoLines :: Maybe Bool
  -- ^ Wrap each output line as an info-line gophermap item ('iLINE\\t\\t\\t\\r\\n').
  -- Use when the gateway is reached via a menu-typed link and the process emits
  -- plain text that needs to render cleanly inside a gophermap.
  , stream      :: Maybe Bool
  -- ^ Pipe the process's stdout straight to the client via 'StreamingResponse'
  -- instead of buffering the full output. Constant memory; the producer paces
  -- the response. The child is terminated if the client disconnects.
  , preamble    :: Maybe [T.Text]
  , postamble   :: Maybe [T.Text]
  } deriving (Show, Eq, Generic)

-- | Configuration for a built-in file server.
data FilesConfig = FilesConfig
  { selector   :: T.Text
  , path       :: FilePath
  , runScripts :: Maybe Bool
  -- ^ When 'Just' 'True', files matching a registered @[[script_extension]]@
  -- under this root are executed instead of served as source. Defaults to
  -- 'False'; opt-in is per-files-root for safety.
  } deriving (Show, Eq, Generic)

-- | A file-extension-driven script runner. When 'runScripts' is enabled on
-- a 'FilesConfig' and a request resolves to a file whose extension matches,
-- the configured command is invoked with the file path substituted into
-- @$file@ (and the request query into @$search@); the process output
-- becomes the response.
data ScriptExtensionConfig = ScriptExtensionConfig
  { extension   :: T.Text
  -- ^ Extension to match, without the leading dot (e.g. @"lhs"@).
  , command     :: T.Text
  , arguments   :: [T.Text]
  -- ^ Argument template; @$file@ is replaced with the canonical absolute
  -- file path, @$search@ with the request query (if any).
  , stream      :: Maybe Bool
  , asInfoLines :: Maybe Bool
  } deriving (Show, Eq, Generic)

-- | Override for the gopher item-type character used when an
-- auto-generated directory listing emits a link to a file with the matching
-- extension. Wins over both the @[[script_extension]]@ default and the
-- hardcoded fallback in 'Venusia.FileHandler.fileExtensionToItemType'.
data FileTypeConfig = FileTypeConfig
  { extension :: T.Text
  , itemType  :: T.Text
  -- ^ One-character gopher item-type code as a string (e.g. @"0"@, @"1"@,
  -- @"9"@, @"I"@). Anything other than length-1 is ignored at use time.
  } deriving (Show, Eq, Generic)


-- TOML Parsing Codecs ---

-- | Codec for the top-level RoutesConfig.
-- This was the main source of the first error. Toml.genericCodec can't be used
-- on the top-level type, so we build it from the individual list codecs.
routesConfigCodec :: TomlCodec RoutesConfig
routesConfigCodec = RoutesConfig
    <$> Toml.list gatewayConfigCodec         "gateway"          .= (.gateways)
    <*> Toml.list filesConfigCodec           "files"            .= (.files)
    <*> Toml.list scriptExtensionConfigCodec "script_extension" .= (.scriptExtensions)
    <*> Toml.list fileTypeConfigCodec        "file_type"        .= (.fileTypes)

-- | Codec for a single command gateway configuration.
gatewayConfigCodec :: TomlCodec GatewayConfig
gatewayConfigCodec = GatewayConfig
    <$> Toml.text     "selector"                                  .= (.selector)
    <*> Toml.text     "command"                                   .= (.command)
    <*> Toml.arrayOf  Toml._Text "arguments"                      .= (.arguments)
    <*> Toml.bool     "wildcard"                                  .= (.wildcard)
    <*> Toml.dioptional (Toml.bool "as_info_lines")               .= (.asInfoLines)
    <*> Toml.dioptional (Toml.bool "stream")                      .= (.stream)
    <*> Toml.dioptional (Toml.arrayOf Toml._Text "preamble")      .= (.preamble)
    <*> Toml.dioptional (Toml.arrayOf Toml._Text "postamble")     .= (.postamble)

-- | Codec for a single file server configuration.
filesConfigCodec :: TomlCodec FilesConfig
filesConfigCodec = FilesConfig
    <$> Toml.text   "selector"               .= (.selector)
    <*> Toml.string "path"                   .= (.path)
    <*> Toml.dioptional (Toml.bool "run_scripts") .= (.runScripts)

-- | Codec for a single script-extension specification.
scriptExtensionConfigCodec :: TomlCodec ScriptExtensionConfig
scriptExtensionConfigCodec = ScriptExtensionConfig
    <$> Toml.text    "extension"                          .= (.extension)
    <*> Toml.text    "command"                            .= (.command)
    <*> Toml.arrayOf Toml._Text "arguments"               .= (.arguments)
    <*> Toml.dioptional (Toml.bool "stream")              .= (.stream)
    <*> Toml.dioptional (Toml.bool "as_info_lines")       .= (.asInfoLines)

-- | Codec for a single file-type override.
fileTypeConfigCodec :: TomlCodec FileTypeConfig
fileTypeConfigCodec = FileTypeConfig
    <$> Toml.text "extension" .= (.extension)
    <*> Toml.text "item_type" .= (.itemType)


-- Route Loading and Building ---

-- | Reads the TOML config and builds a list of all Gopher routes.
loadRoutes :: FilePath -> T.Text -> Int -> IO [Route]
loadRoutes path host port = do
  eConfig <- readRoutesConfig path
  case eConfig of
    Left err -> do
      putStrLn $ "Error loading route configuration: " ++ err
      putStrLn "No routes loaded."
      return []
    Right config -> do
      let routes = buildRoutes config host port
      putStrLn $ "Successfully loaded " ++ show (length routes) ++ " routes."
      return routes

-- | Reads and decodes the routes configuration from a TOML file.
readRoutesConfig :: FilePath -> IO (Either String RoutesConfig)
readRoutesConfig path = catchIOError
  (do
    contents <- TIO.readFile path
    case Toml.decode routesConfigCodec contents of
      Left err      -> pure . Left $ "Failed to parse TOML: " ++ show err
      Right config -> do
        -- Optional: Log what was loaded
        forM_ (config.gateways) $ \c ->
          putStrLn $ "Loaded gateway: " ++ T.unpack c.selector
        forM_ (config.files)    $ \c -> putStrLn $ "Loaded files: " ++ T.unpack c.selector
        forM_ (config.scriptExtensions) $ \c ->
          putStrLn $ "Loaded script_extension: ." ++ T.unpack c.extension
        forM_ (config.fileTypes) $ \c ->
          putStrLn $ "Loaded file_type: ." ++ T.unpack c.extension ++ " -> " ++ T.unpack c.itemType
        pure $ Right config
  )
  (\e -> pure . Left $ "Error reading file: " ++ show e)

-- | Builds a list of routes from the parsed RoutesConfig.
-- This function was fixed to handle all three route types and pass parameters correctly.
buildRoutes :: RoutesConfig -> T.Text -> Int -> [Route]
buildRoutes config host port =
    (buildGatewayRoutes $ config.gateways) ++
    (buildFileRoutes    (config.files)    config.scriptExtensions config.fileTypes host port)

-- | Builds routes for command gateways.
buildGatewayRoutes :: [GatewayConfig] -> [Route]
buildGatewayRoutes = concatMap createGatewayRoute
  where
    createGatewayRoute config =
      let handler = createCommandHandler config
      in if config.wildcard
          then [onWildcard config.selector handler]
          else [on config.selector handler]

-- | Builds routes for file servers.
buildFileRoutes
  :: [FilesConfig] -> [ScriptExtensionConfig] -> [FileTypeConfig]
  -> T.Text -> Int -> [Route]
buildFileRoutes configs scriptExts fileTypes host port =
    concatMap mkRoute configs
  where
    mkRoute config =
      [onWildcard config.selector (createFileHandler config scriptExts fileTypes host port)]


-- Handler Creation ---

-- | Creates a handler for a file server configuration.
createFileHandler
  :: FilesConfig -> [ScriptExtensionConfig] -> [FileTypeConfig]
  -> T.Text -> Int -> Handler
createFileHandler config scriptExts fileTypes host port request =
  case request.reqWildcard of
    Just wildcard -> do
      let scriptsEnabled = fromMaybe False config.runScripts
          fileHook = if scriptsEnabled
                       then mkScriptHook scriptExts request.reqQuery
                       else \_ -> pure Nothing
          itemTypeFn = resolveItemType fileTypes scriptExts
      serveDirectoryWith host port config.path config.selector wildcard Nothing
        fileHook itemTypeFn
    Nothing       -> pure $ TextResponse "Error: No path provided for file handler."

-- | Build a file hook that consults the script-extension registry. The
-- returned hook short-circuits 'serveDirectoryWith' when the requested
-- file's extension is registered for execution; otherwise it returns
-- 'Nothing' and the file is served verbatim.
--
-- The hook captures the request's @$search@ query so scripts invoked
-- type-7-style see the user's input.
mkScriptHook
  :: [ScriptExtensionConfig] -> Maybe T.Text -> FilePath -> IO (Maybe Response)
mkScriptHook specs mQuery filePath =
  case lookupExt (extKey filePath) specs of
    Nothing   -> pure Nothing
    Just spec -> do
      let processedArgs =
            map (T.unpack . substituteScriptArg (T.pack filePath) mQuery) spec.arguments
      Just <$> runProcess
                 (fromMaybe False spec.stream)
                 (fromMaybe False spec.asInfoLines)
                 (T.unpack spec.command)
                 processedArgs
                 (Just (takeDirectory filePath))
                 [] []

-- | Pick a directory-listing item-type character for a file path.
--
-- Resolution order:
--
-- 1. @[[file_type]]@ override for that extension, if defined.
-- 2. Otherwise: if @[[script_extension]]@ is defined, @\'1\'@ when
--    @as_info_lines = true@, else @\'0\'@.
-- 3. Otherwise: 'Venusia.FileHandler.fileExtensionToItemType' (the
--    hardcoded fallback table).
resolveItemType :: [FileTypeConfig] -> [ScriptExtensionConfig] -> FilePath -> Char
resolveItemType fileTypes scriptExts path =
  case lookupExt key fileTypes >>= singleChar . (.itemType) of
    Just c  -> c
    Nothing -> case lookupExt key scriptExts of
      Just spec -> if fromMaybe False spec.asInfoLines then '1' else '0'
      Nothing   -> fileExtensionToItemType path
  where
    key = extKey path

-- | Filename → @"lhs"@-style key: lowercase extension without the dot, or
-- empty if there isn't one. Used for both 'mkScriptHook' and
-- 'resolveItemType'. Lowercasing here (and in 'lookupExt') means
-- @\"digest.LHS\"@ matches a registered @extension = \"lhs\"@; the
-- filesystem stays case-sensitive but the registry lookup is normalised.
extKey :: FilePath -> T.Text
extKey p = case takeExtension p of
  ('.':rest) -> T.toLower (T.pack rest)
  _          -> T.empty

-- | Look up an extension-keyed record. Polymorphic in the record type so
-- the same lookup works for 'FileTypeConfig' and 'ScriptExtensionConfig';
-- the constraint demands an @extension :: T.Text@ field. Case-insensitive
-- on the registered side too, so @extension = \"LHS\"@ in TOML is fine.
lookupExt
  :: HasField "extension" c T.Text => T.Text -> [c] -> Maybe c
lookupExt _ [] = Nothing
lookupExt e (c : cs)
  | T.toLower (getField @"extension" c) == e = Just c
  | otherwise                                = lookupExt e cs

-- | A 'T.Text' that's exactly one character long, returned as 'Just' that
-- character; anything else is 'Nothing' (and is silently skipped by the
-- item-type resolver).
singleChar :: T.Text -> Maybe Char
singleChar t = case T.uncons t of
  Just (c, rest) | T.null rest -> Just c
  _                            -> Nothing

-- | Replace @$file@ and @$search@ in a script-extension argument template.
-- (No @$wildcard@: the file path itself is what the wildcard matched.)
substituteScriptArg :: T.Text -> Maybe T.Text -> T.Text -> T.Text
substituteScriptArg filePath mQuery =
    T.replace "$file" filePath
      . T.replace "$search" (fromMaybe "" mQuery)

-- | Creates a handler for a command gateway configuration.
createCommandHandler :: GatewayConfig -> Handler
createCommandHandler config request =
  let
    args = config.arguments
    -- NOTE: so much un/repacking! why converting to string, here, also?
    processedArgs = map (substituteArgPlaceholders request.reqQuery request.reqWildcard . T.unpack) args
    -- A search is implied if the $search placeholder is used.
    isSearch = any (T.isInfixOf "$search") args
    canExecute = not (isSearch && request.reqQuery == Nothing)
    preamble = fromMaybe [] $
      map (substitutePlaceholders request.reqQuery request.reqWildcard) <$> config.preamble
    postamble = fromMaybe [] $
      map (substitutePlaceholders request.reqQuery request.reqWildcard) <$> config.postamble
    asInfoLinesFlag = fromMaybe False config.asInfoLines
    streamFlag = fromMaybe False config.stream
  in
    if canExecute
      then runProcess streamFlag asInfoLinesFlag (T.unpack config.command) processedArgs Nothing preamble postamble
      else pure $ TextResponse $ error' "A search query is required for this gateway."


-- Process Execution and Helpers ---

-- | Replace $wildcard and $search appropriately.
substitutePlaceholders :: Maybe T.Text -> Maybe T.Text -> T.Text -> T.Text
substitutePlaceholders searchValue wildcardValue =
    T.replace "$wildcard" (fromMaybe "" wildcardValue)
      . T.replace "$search" (fromMaybe "" searchValue)

-- | Substitute placeholders like $search and $wildcard in command arguments.
substituteArgPlaceholders :: Maybe T.Text -> Maybe T.Text -> String -> String
substituteArgPlaceholders searchValue wildcardValue =
    T.unpack . substitutePlaceholders searchValue wildcardValue . T.pack

{- | Run an external process and return a 'Response'.

The shape of the response is selected by two orthogonal booleans:

* @stream@:        'False' buffers the full stdout into a 'TextResponse';
                   'True' pipes stdout through a 'StreamingResponse' (constant
                   memory). The child process is terminated via 'bracket' if
                   the producer ends, throws, or the client disconnects.
* @asInfoLines@:   'False' passes stdout through verbatim (raw text or raw
                   bytes); 'True' wraps each output line as an info-line
                   gophermap item ('iLINE\\t\\t\\t\\r\\n') so the response
                   renders cleanly inside a gopher menu.

@preamble@ and @postamble@ are emitted before/after the process output. They
are passed through verbatim and are most useful when @asInfoLines = True@,
where they let you frame the wrapped output with real gophermap items
(headers, navigation links).

Exported so that library users can build streaming-process gateways without
going through the TOML loader.
-}
runProcess
  :: Bool          -- ^ stream
  -> Bool          -- ^ asInfoLines
  -> FilePath      -- ^ command
  -> [String]      -- ^ arguments
  -> Maybe FilePath -- ^ working directory
  -> [T.Text]      -- ^ preamble lines (already placeholder-substituted)
  -> [T.Text]      -- ^ postamble lines (already placeholder-substituted)
  -> IO Response
runProcess streamFlag asInfoLinesFlag cmd args mCwd pre post =
  -- When wrapping output as a gophermap, normalise framing on
  -- preamble/postamble entries: each must end with \r\n or it gets glued
  -- onto whatever follows (next info-line item, or the gopher terminator).
  -- Skip normalisation in raw-output cells so a binary preamble (rare,
  -- but legal) isn't corrupted with extra bytes.
  let pre'  = if asInfoLinesFlag then map ensureCRLF pre  else pre
      post' = if asInfoLinesFlag then map ensureCRLF post else post
  in case (streamFlag, asInfoLinesFlag) of
       (False, False) -> readProcessRaw      cmd args mCwd pre' post'
       (False, True ) -> readProcessInfoWrap cmd args mCwd pre' post'
       (True,  False) -> pure (streamProcessRaw      cmd args mCwd pre' post')
       (True,  True ) -> pure (streamProcessInfoWrap cmd args mCwd pre' post')

-- | Append a trailing @\\r\\n@ if the line doesn't already have one. Used
-- to normalise gophermap-context preamble/postamble entries.
ensureCRLF :: T.Text -> T.Text
ensureCRLF t
  | "\r\n" `T.isSuffixOf` t = t
  | otherwise               = t <> "\r\n"

-- | Run a command, capturing all stdout into a buffered 'TextResponse'.
readProcessRaw
  :: FilePath -> [String] -> Maybe FilePath -> [T.Text] -> [T.Text] -> IO Response
readProcessRaw cmd args mCwd pre post = do
  result <- try (P.readCreateProcess ((P.proc cmd args) { P.cwd = mCwd }) "")
              :: IO (Either SomeException String)
  let body = case result of
        Left err  -> [error' . T.pack $ "Process execution failed: " ++ show err]
        Right out -> [T.pack out]
  pure . TextResponse . render $ pre ++ body ++ post

-- | Run a command, wrapping every line of stdout as an info-line gophermap item.
readProcessInfoWrap
  :: FilePath -> [String] -> Maybe FilePath -> [T.Text] -> [T.Text] -> IO Response
readProcessInfoWrap cmd args mCwd pre post = do
  result <- try (P.readCreateProcess ((P.proc cmd args) { P.cwd = mCwd }) "")
              :: IO (Either SomeException String)
  let body = case result of
        Left err  -> [error' . T.pack $ "Process execution failed: " ++ show err]
        Right out -> info . T.pack <$> lines out
  pure . TextResponse . render $ pre ++ body ++ post

-- | Pipe a process's stdout straight to the client. Raw bytes; binary-safe.
-- No gopher terminator is emitted (callers expecting type-1 menus should use
-- 'streamProcessInfoWrap').
streamProcessRaw
  :: FilePath -> [String] -> Maybe FilePath -> [T.Text] -> [T.Text] -> Response
streamProcessRaw cmd args mCwd pre post = StreamingResponse $ \send ->
  bracket
    (P.createProcess (procSpec cmd args mCwd))
    cleanupProcess
    $ \(_, mhOut, _, _) -> do
        forM_ pre  $ \l -> send (TE.encodeUtf8 l)
        case mhOut of
          Just h  -> hSetBinaryMode h True >> streamFromHandle h send
          Nothing -> pure ()
        forM_ post $ \l -> send (TE.encodeUtf8 l)

-- | Stream a process's stdout, wrapping each line as an info-line gophermap
-- item. Sends the gopher terminator (".\\r\\n") after the postamble.
streamProcessInfoWrap
  :: FilePath -> [String] -> Maybe FilePath -> [T.Text] -> [T.Text] -> Response
streamProcessInfoWrap cmd args mCwd pre post = StreamingResponse $ \send ->
  bracket
    (P.createProcess (procSpec cmd args mCwd))
    cleanupProcess
    $ \(_, mhOut, _, _) -> do
        forM_ pre  $ \l -> send (TE.encodeUtf8 l)
        case mhOut of
          Just h  -> do
            -- Force UTF-8 so 'TIO.hGetLine' doesn't bomb on non-ASCII
            -- output when the daemon's locale isn't UTF-8.
            hSetEncoding h utf8
            wrapLines h send
          Nothing -> pure ()
        forM_ post $ \l -> send (TE.encodeUtf8 l)
        send (TE.encodeUtf8 ".\r\n")
  where
    wrapLines :: Handle -> (BS.ByteString -> IO ()) -> IO ()
    wrapLines h send = do
      eof <- hIsEOF h
      unless eof $ do
        rawLine <- TIO.hGetLine h
        -- 'hGetLine' strips the LF but keeps a trailing CR (CRLF input);
        -- leaving it in place would corrupt the info-line gophermap item.
        let line = fromMaybe rawLine (T.stripSuffix "\r" rawLine)
        send (TE.encodeUtf8 (info line))
        wrapLines h send

procSpec :: FilePath -> [String] -> Maybe FilePath -> P.CreateProcess
procSpec cmd args mCwd =
  (P.proc cmd args)
    { P.std_in  = P.NoStream     -- script must not read the daemon's stdin
    , P.std_out = P.CreatePipe
    , P.std_err = P.Inherit
    , P.cwd     = mCwd
    }

-- | Microseconds to wait for a child to honour SIGTERM before falling back
-- to SIGKILL. Tight enough that a stuck script can't pin a connection
-- thread for long; generous enough that a well-behaved script finishes
-- its cleanup (flush logs, close files) on the graceful path.
{-@ cleanupGracePeriod :: {v:Int | v > 0} @-}
cleanupGracePeriod :: Int
cleanupGracePeriod = 2 * 1000 * 1000  -- 2 seconds

cleanupProcess
  :: (Maybe a, Maybe b, Maybe c, P.ProcessHandle) -> IO ()
cleanupProcess (_, _, _, ph) = do
  -- Step 1: graceful (SIGTERM on Unix; hard kill on Windows).
  P.terminateProcess ph `catch` (\(_ :: SomeException) -> pure ())
  -- Step 2: bounded wait. A child that catches/ignores SIGTERM must not
  -- be allowed to pin this connection thread forever.
  mExit <- timeout cleanupGracePeriod (P.waitForProcess ph)
  case mExit of
    Just _  -> pure ()
    Nothing -> do
      -- Step 3: SIGKILL. Uninterruptible; cannot be caught or ignored.
      mPid <- P.getPid ph
      forM_ mPid $ \pid ->
        Posix.signalProcess Posix.sigKILL pid
          `catch` (\(_ :: SomeException) -> pure ())
      -- Step 4: reap the zombie.
      _ <- P.waitForProcess ph
      pure ()
