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
  , splitForPathInfo
  , ScriptExtensionConfig (..)
  , FileTypeConfig (..)
  , FilesConfig (..)
  , RoutesConfig (..)
  , routesConfigCodec
  ) where

import           Control.Exception          (SomeException, bracket, catch, try)
import qualified Data.ByteString            as BS
import           Data.List                  (isPrefixOf)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Maybe                 (fromMaybe)
import qualified System.Process             as P
import           System.Directory           (canonicalizePath, doesFileExist)
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

import           System.FilePath            (splitDirectories, takeDirectory,
                                             takeExtension, (</>))

import           Venusia.MenuBuilder        (error', info, render)
import           Venusia.Server             (Handler, Request (..), Response (..),
                                             Route, on, onWildcard, streamFromHandle)
import           Venusia.FileHandler        (fileExtensionToItemType, serveDirectoryWith)

-- Configuration Data Types ---

-- | Top-level configuration. Note: as of 0.7.0.0 there is no top-level
-- @[[script_extension]]@ — script-runner specs live inside their owning
-- @[[files]]@ block (see 'FilesConfig'). @[[file_type]]@ stays at the top
-- level for genuinely global rules; @[[files]]@ blocks may *also* declare
-- nested @[[files.file_type]]@ entries that shadow globals within that
-- block's directory listings.
data RoutesConfig = RoutesConfig
  { gateways  :: [GatewayConfig]
  -- ^ Command gateway configurations.
  , files     :: [FilesConfig]
  -- ^ File server configurations. Each may carry its own per-block
  -- @[[files.script_extension]]@ and @[[files.file_type]]@ rules.
  , fileTypes :: [FileTypeConfig]
  -- ^ Global per-extension overrides for the gopher item-type character used
  -- in auto-generated directory listings. Any @[[files.file_type]]@ on the
  -- serving @[[files]]@ block wins over these.
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
--
-- The two nested-rules fields ('scriptExtensions' and 'fileTypes') were added
-- in 0.7.0.0. They replace the old top-level @[[script_extension]]@ pool plus
-- the @run_scripts@ flag: a @[[files]]@ block executes scripts iff it has a
-- matching @[[files.script_extension]]@ entry. Default-deny by construction.
data FilesConfig = FilesConfig
  { selector         :: T.Text
  , path             :: FilePath
  , scriptExtensions :: [ScriptExtensionConfig]
  -- ^ Per-block script-runner specs (TOML: @[[files.script_extension]]@).
  -- A file under this @[[files]]@'s @path@ whose extension matches one of
  -- these is executed by the configured runner instead of being served as
  -- source. Empty list = no execution. There is no top-level fallback.
  , fileTypes        :: [FileTypeConfig]
  -- ^ Per-block item-type overrides (TOML: @[[files.file_type]]@). Win over
  -- top-level @[[file_type]]@ when this block is generating the listing.
  , allowDotfiles    :: Maybe Bool
  -- ^ TOML: @allow_dotfiles@. Whether this @[[files]]@ block will
  -- expose Unix-style dotfiles to clients /at all/ — covers both
  -- listing visibility and direct-selector access. Defaults to
  -- 'False' (refuse), so a request for @/foo/.env@ is rejected with
  -- a type-3 error response even when the client knows the exact
  -- path. This is defence-in-depth: hiding from the listing alone
  -- isn't safety, because an attacker who guesses the filename can
  -- still fetch it. Set to 'True' on a block whose served content
  -- really is dotfiles.
  --
  -- The block's configured 'indexFile' is /always/ exempt — it's
  -- the framework's directory-menu source, named-by-convention even
  -- when that convention is a dotfile.
  , indexFile        :: Maybe FilePath
  -- ^ TOML: @index_file@. Filename Venusia reads to render a
  -- directory listing's menu, and the single dotfile name (if it is
  -- one) exempt from the @allow_dotfiles@ refusal. Defaults to
  -- @.gophermap@. Change it if your directory-menu files live under
  -- a non-dotfile or differently-named convention (e.g.
  -- @index.gph@). Whatever it's set to, that filename is what both
  -- the auto-rendering and the dotfile-exception apply to.
  } deriving (Show, Eq, Generic)

-- | A file-extension-driven script runner. When a request resolves to a file
-- whose extension matches a 'ScriptExtensionConfig' in the serving
-- 'FilesConfig'.scriptExtensions list, the configured command is invoked
-- with the request's substitution tokens expanded into 'arguments' (see
-- 'substituteScriptArg' for the supported tokens); the process output
-- becomes the response.
data ScriptExtensionConfig = ScriptExtensionConfig
  { extension   :: T.Text
  -- ^ Extension to match, without the leading dot (e.g. @"lhs"@).
  , command     :: T.Text
  , arguments   :: [T.Text]
  -- ^ Argument template. Each entry is run through 'substituteScriptArg'
  -- before exec; the supported tokens are @$file@, @$selector@, @$search@,
  -- and @$pathinfo@.
  , stream      :: Maybe Bool
  , asInfoLines :: Maybe Bool
  } deriving (Show, Eq, Generic)

-- | Override for the gopher item-type character used when an
-- auto-generated directory listing emits a link to a file with the matching
-- extension. Wins over the @[[script_extension]]@-derived default and over
-- the hardcoded fallback in 'Venusia.FileHandler.fileExtensionToItemType'.
data FileTypeConfig = FileTypeConfig
  { extension :: T.Text
  , itemType  :: T.Text
  -- ^ One-character gopher item-type code as a string (e.g. @"0"@, @"1"@,
  -- @"9"@, @"I"@). Anything other than length-1 is ignored at use time.
  } deriving (Show, Eq, Generic)


-- TOML Parsing Codecs ---

-- | Codec for the top-level RoutesConfig.
routesConfigCodec :: TomlCodec RoutesConfig
routesConfigCodec = RoutesConfig
    <$> Toml.list gatewayConfigCodec  "gateway"   .= (.gateways)
    <*> Toml.list filesConfigCodec    "files"     .= (.files)
    <*> Toml.list fileTypeConfigCodec "file_type" .= (.fileTypes)

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
--
-- Note the nested @Toml.list@ calls — TOML arrays-of-tables nest cleanly
-- (e.g. @[[files.script_extension]]@), and tomland's @Toml.list@ codec
-- works at any depth.
filesConfigCodec :: TomlCodec FilesConfig
filesConfigCodec = FilesConfig
    <$> Toml.text   "selector"                                            .= (.selector)
    <*> Toml.string "path"                                                .= (.path)
    <*> Toml.list scriptExtensionConfigCodec "script_extension"           .= (.scriptExtensions)
    <*> Toml.list fileTypeConfigCodec        "file_type"                  .= (.fileTypes)
    <*> Toml.dioptional (Toml.bool "allow_dotfiles")                      .= (.allowDotfiles)
    <*> Toml.dioptional (Toml.string "index_file")                        .= (.indexFile)

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
        forM_ (config.gateways) $ \c ->
          putStrLn $ "Loaded gateway: " ++ T.unpack c.selector
        forM_ (config.files)    $ \c -> do
          putStrLn $ "Loaded files: " ++ T.unpack c.selector
          forM_ c.scriptExtensions $ \s ->
            putStrLn $ "  + script_extension: ." ++ T.unpack s.extension
          forM_ c.fileTypes $ \f ->
            putStrLn $ "  + file_type: ." ++ T.unpack f.extension ++ " -> " ++ T.unpack f.itemType
        forM_ (config.fileTypes) $ \c ->
          putStrLn $ "Loaded global file_type: ." ++ T.unpack c.extension ++ " -> " ++ T.unpack c.itemType
        pure $ Right config
  )
  (\e -> pure . Left $ "Error reading file: " ++ show e)

-- | Builds a list of routes from the parsed RoutesConfig.
buildRoutes :: RoutesConfig -> T.Text -> Int -> [Route]
buildRoutes config host port =
    (buildGatewayRoutes $ config.gateways) ++
    (buildFileRoutes    (config.files)    config.fileTypes host port)

-- | Builds routes for command gateways.
buildGatewayRoutes :: [GatewayConfig] -> [Route]
buildGatewayRoutes = concatMap createGatewayRoute
  where
    createGatewayRoute config =
      let handler = createCommandHandler config
      in if config.wildcard
          then [onWildcard config.selector handler]
          else [on config.selector handler]

-- | Builds routes for file servers. Global @[[file_type]]@ rules are passed
-- in for use as a fallback when a 'FilesConfig' has no nested override for
-- the requested extension.
buildFileRoutes
  :: [FilesConfig] -> [FileTypeConfig]
  -> T.Text -> Int -> [Route]
buildFileRoutes configs globalFileTypes host port =
    concatMap mkRoute configs
  where
    mkRoute config =
      [onWildcard config.selector (createFileHandler config globalFileTypes host port)]


-- Handler Creation ---

-- | Creates a handler for a file server configuration.
--
-- Resolution at request time:
--
-- * Script execution is gated entirely by @config.scriptExtensions@. Empty
--   list = the file is served as static content. There is no fallback to a
--   global pool (intentional default-deny — a misconfigured @[[files]]@
--   block can never accidentally execute scripts).
-- * Item-type resolution merges the block's nested file_type rules with the
--   provided global rules; nested wins via list ordering ('lookupExt' takes
--   the first match).
createFileHandler
  :: FilesConfig -> [FileTypeConfig]
  -> T.Text -> Int -> Handler
createFileHandler config globalFileTypes host port request =
  case request.reqWildcard of
    Just wildcard -> do
      let scriptExts      = config.scriptExtensions
          mergedFileTypes = config.fileTypes ++ globalFileTypes
          itemTypeFn      = resolveItemType mergedFileTypes scriptExts
      -- When the block has no script extensions, scripts are disabled;
      -- skip the path-info walk entirely (no canonicalise/stat overhead)
      -- and serve as static content. Otherwise split the wildcard on the
      -- first script-extension boundary so a request like
      -- @wiki.lhs/Page/SubPage@ runs @wiki.lhs@ with @\/Page\/SubPage@
      -- threaded into @$pathinfo@.
      let allowDots = fromMaybe False        config.allowDotfiles
          idxFile   = fromMaybe ".gophermap" config.indexFile
      if null scriptExts
        then serveDirectoryWith host port config.path config.selector wildcard
               Nothing (\_ -> pure Nothing) itemTypeFn allowDots idxFile
        else do
          (scriptWildcard, pathInfo) <-
            splitForPathInfo config.path scriptExts wildcard
          let fileHook = mkScriptHook scriptExts request.reqSelector
                                      request.reqQuery pathInfo
                                      request.reqClientIp
          serveDirectoryWith host port config.path config.selector scriptWildcard
            Nothing fileHook itemTypeFn allowDots idxFile
    Nothing       -> pure $ TextResponse "Error: No path provided for file handler."

-- | Detect a path-info boundary in the request wildcard.
--
-- Walks the wildcard left-to-right one @\/@-separated segment at a time. The
-- first segment whose extension matches a registered 'ScriptExtensionConfig'
-- /and/ whose accumulated prefix resolves to a real regular file under
-- @root@ becomes the split point: everything up to and including that
-- segment is the script-prefix wildcard; everything after becomes the
-- path-info string.
--
-- * No match → @(wildcard, \"\")@. Behaviour is identical to passing the
--   full wildcard straight through to 'serveDirectoryWith'.
-- * Match with no remainder (@\"script.lhs\"@) → @(\"script.lhs\", \"\")@.
-- * Match with empty trailing segment (@\"script.lhs\/\"@) →
--   @(\"script.lhs\", \"\/\")@. The trailing slash is preserved as a
--   distinguishable path-info value, matching CGI's @PATH_INFO@ convention.
-- * Match with remainder (@\"script.lhs\/foo\/bar\"@) →
--   @(\"script.lhs\", \"\/foo\/bar\")@.
--
-- For each candidate the walk canonicalises and bounds-checks the prefix
-- against @root@ /before/ touching the filesystem with 'doesFileExist', so
-- a wildcard with @..@ traversals cannot be used to probe for files
-- outside the served root. 'serveDirectoryWith' will re-canonicalise and
-- re-check the script prefix it ultimately receives, so the path-info
-- bytes never participate in disk access — they reach only the script,
-- just like @$search@.
splitForPathInfo
  :: FilePath
  -> [ScriptExtensionConfig]
  -> T.Text
  -> IO (T.Text, T.Text)
splitForPathInfo root scriptExts wildcard = do
  rootC <- canonicalizePath root
  let segs       = T.splitOn "/" wildcard
      candidates = [ i | (i, s) <- zip [0 ..] segs, hasScriptExt s ]

      hasScriptExt s = case lookupExt (extKey (T.unpack s)) scriptExts of
        Just _  -> True
        Nothing -> False

      tryCandidates [] = pure (wildcard, T.empty)
      tryCandidates (i : is) = do
        let prefixSegs = take (i + 1) segs
            restSegs   = drop (i + 1) segs
            prefixText = T.intercalate "/" prefixSegs
            relText    = T.dropWhile (== '/') prefixText
            diskPath   = root </> T.unpack relText
        pathC <- canonicalizePath diskPath
        if not (splitDirectories rootC `isPrefixOf` splitDirectories pathC)
          then tryCandidates is
          else do
            isFile <- doesFileExist diskPath
            if not isFile
              then tryCandidates is
              else
                let pathInfo = case restSegs of
                      [] -> T.empty
                      rs -> "/" <> T.intercalate "/" rs
                in pure (prefixText, pathInfo)

  tryCandidates candidates

-- | Build a file hook that consults the script-extension registry. The
-- returned hook short-circuits 'serveDirectoryWith' when the requested
-- file's extension is registered for execution; otherwise it returns
-- 'Nothing' and the file is served verbatim.
--
-- The hook captures the gopher selector that resolved to this script (for
-- @$selector@ substitution so the script can generate menu items linking
-- back to itself without hardcoding its own path), the request's @$search@
-- query (so type-7-style invocations see the user's input), the
-- @$pathinfo@ string carved out of the wildcard by 'splitForPathInfo'
-- (empty when the request did not address a virtual sub-path under the
-- script), and the client's @$remote_ip@ (empty when the peer couldn't be
-- determined).
mkScriptHook
  :: [ScriptExtensionConfig]
  -> T.Text                   -- ^ request selector (for @$selector@ substitution)
  -> Maybe T.Text             -- ^ optional search query (for @$search@)
  -> T.Text                   -- ^ path-info suffix (for @$pathinfo@); @\"\"@ when none
  -> T.Text                   -- ^ client IP (for @$remote_ip@); @\"\"@ when unknown
  -> FilePath
  -> IO (Maybe Response)
mkScriptHook specs reqSelector mQuery pathInfo clientIp filePath =
  case lookupExt (extKey filePath) specs of
    Nothing   -> pure Nothing
    Just spec -> do
      let processedArgs =
            map (T.unpack . substituteScriptArg (T.pack filePath) reqSelector mQuery pathInfo clientIp)
                spec.arguments
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
-- 1. @[[file_type]]@ override for that extension, if defined. Caller is
--    expected to have already merged nested-then-global rules so the first
--    match wins correctly.
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

-- | Replace @$file@, @$selector@, @$search@, and @$pathinfo@ in a
-- script-extension argument template.
--
-- * @$file@ — canonical absolute path to the script on disk.
-- * @$selector@ — gopher selector that resolved to this script, e.g.
--   @/cgi/figlet.lhs@. Useful for emitting menu items that link back to
--   the script itself (search prompts, refresh links) without hardcoding
--   the path.
-- * @$search@ — the request's query string after the tab, or empty.
-- * @$pathinfo@ — the portion of the selector after the script filename
--   (e.g. @\/Page\/SubPage@ for selector @\/cgi\/wiki.lhs\/Page\/SubPage@),
--   with a leading slash. Empty when the request addressed the script
--   directly. Modeled on CGI's @PATH_INFO@.
-- * @$remote_ip@ — the connecting client's IP address as text (IPv4
--   dotted-quad or IPv6 colon form). Empty when the peer can't be
--   looked up. Useful for rate limiting, per-IP rule application, or
--   audit logging — whatever the script wants to do with it is the
--   script's call; this just plumbs the value through.
substituteScriptArg :: T.Text -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> T.Text -> T.Text
substituteScriptArg filePath reqSelector mQuery pathInfo clientIp =
    T.replace "$file"      filePath
      . T.replace "$selector"  reqSelector
      . T.replace "$search"    (fromMaybe "" mQuery)
      . T.replace "$pathinfo"  pathInfo
      . T.replace "$remote_ip" clientIp

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
