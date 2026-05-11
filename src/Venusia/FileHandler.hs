{- | A default handler which will serve a directory listing.

Has support for .gophermap files. If present in the directory, it will be displayed
instead of the directory listing.

Note that .gophermap files allow for shorthand where the host and port don't need to be
specified (assume it from server config).

-}

module Venusia.FileHandler
  ( serveDirectory
  , serveDirectoryWith
  , fileExtensionToItemType
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
  ( listDirectory
  , doesFileExist
  , doesDirectoryExist
  , canonicalizePath
  , getFileSize
  , getModificationTime
  , pathIsSymbolicLink
  )
import Venusia.MenuBuilder (gophermapRender, render, item, error', info, menu, directory, text)
import Venusia.Server (Request(..), Response(..))
import System.FilePath ((</>), takeExtension, takeFileName, makeRelative, takeDirectory, splitDirectories)
import qualified Data.ByteString as BL
import Data.List (dropWhileEnd, intercalate, isPrefixOf, sortBy, partition)
import Data.Time (UTCTime, formatTime, defaultTimeLocale)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Ord (comparing)
import Text.Printf (printf)
import System.Posix.Files (getFileStatus, fileSize, isSymbolicLink)
import Control.Exception (try, SomeException)
import Data.Maybe (fromMaybe)

-- Constants for formatting
maxFilenameLength :: Int
maxFilenameLength = 40  -- Maximum characters to display for a filename

sizeColumnWidth :: Int
sizeColumnWidth = 10    -- Width of the size column

-- | Data type for storing file information
data FileInfo = FileInfo
  { fiName :: T.Text
  , fiPath :: FilePath
  , fiIsDir :: Bool
  , fiSize :: Integer
  , fiModTime :: UTCTime
  }

-- | Sorting options for directory listings
data SortBy = ByName | BySize | ByTime
  deriving (Eq, Read, Show)

-- | Format file size in a human-readable format
formatFileSize :: Integer -> String
formatFileSize size
  | size < 1024 = show size ++ "B"
  | size < 1024 * 1024 = printf "%.1fKB" (fromIntegral size / 1024 :: Double)
  | size < 1024 * 1024 * 1024 = printf "%.1fMB" (fromIntegral size / (1024 * 1024) :: Double)
  | otherwise = printf "%.1fGB" (fromIntegral size / (1024 * 1024 * 1024) :: Double)

-- | Determine item type from file extension
fileExtensionToItemType :: FilePath -> Char
fileExtensionToItemType path =
  case takeExtension path of
    ".txt" -> '0'  -- Text file
    ".md"  -> '0'  -- Markdown file
    ".csv" -> '0'  -- CSV file
    ".jpg" -> 'I'  -- Image file
    ".jpeg" -> 'I' -- Image file
    ".bmp" -> 'I'  -- Image file
    ".png" -> 'I'  -- Image file
    ".gif" -> 'I'  -- GIF file
    ".html" -> 'h' -- HTML file
    _      -> '9'  -- Default to binary for unknown types

-- | Get file info for a single file
getFileInfo :: FilePath -> FilePath -> IO FileInfo
getFileInfo basePath fileName = do
  let fullPath = basePath </> fileName
  isDir <- doesDirectoryExist fullPath
  isSymlink <- pathIsSymbolicLink fullPath

  -- Safely get file size, handling symlinks and nonexistent files
  size <- if isDir then
            return 0
          else do
            result <- try $ getFileStatus fullPath
            case result of
              Left (_ :: SomeException) -> return 0  -- If error occurs, return 0
              Right stat -> return $ fromIntegral $ fileSize stat

  -- Safely get modification time
  modTime <- try $ getModificationTime fullPath
  let safeModTime = case modTime of
                      Left (_ :: SomeException) -> read "1970-01-01 00:00:00 UTC"  -- Default time
                      Right time -> time

  -- Add symlink indicator to name if needed
  let displayName = if isSymlink
                    then T.pack $ fileName ++ " -> symlink"
                    else T.pack fileName

  return $ FileInfo
    { fiName = displayName
    , fiPath = fullPath
    , fiIsDir = isDir
    , fiSize = size
    , fiModTime = safeModTime
    }

-- | Sort file info based on the specified parameter
sortFileInfo :: SortBy -> [FileInfo] -> [FileInfo]
sortFileInfo sortCriteria fileInfos =
  -- Always sort directories first, then apply the selected sort criteria
  let
    (dirs, files) = partition (.fiIsDir) fileInfos
    sortedDirs = case sortCriteria of
      ByName -> sortBy (comparing (.fiName)) dirs
      BySize -> dirs  -- Directories all have size 0, so keep original order
      ByTime -> sortBy (comparing (.fiModTime)) dirs
    sortedFiles = case sortCriteria of
      ByName -> sortBy (comparing (.fiName)) files
      BySize -> sortBy (comparing (.fiSize)) files
      ByTime -> sortBy (comparing (.fiModTime)) files
  in
    sortedDirs ++ sortedFiles

-- Modify the formatFileInfoTable function to ensure proper spacing
formatFileInfoTable :: FileInfo -> T.Text
formatFileInfoTable fi =
  let
    -- Add "/" suffix to directory names
    name = if fi.fiIsDir
           then fi.fiName `T.append` T.pack "/"
           else fi.fiName

    -- Truncate filename if too long and pad to fixed width
    displayName = truncateFilename name

    -- Format and pad the size column
    size = T.pack $ formatFileSize fi.fiSize
    paddedSize = T.justifyRight sizeColumnWidth ' ' size

    -- Format date to YYYY-MM-DD only
    time = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" fi.fiModTime
  in
    displayName `T.append` paddedSize `T.append` T.pack "  " `T.append` time

-- Update the truncateFilename function to handle the new length
truncateFilename :: T.Text -> T.Text
truncateFilename name =
  if T.length name <= maxFilenameLength
  then name `T.append` T.replicate (maxFilenameLength - T.length name) (T.singleton ' ')
  else T.take (maxFilenameLength - 4) name `T.append` T.pack "... "

-- FIXME: this is so messy! it's hard to tell what's going on with the file paths.
-- | List directory contents as a gophermap. The supplied item-type function
-- decides the type character for each non-directory file (so callers can
-- override the hardcoded extension table for e.g. script extensions).
listDirectoryAsGophermapWith
  :: T.Text -> Int -> FilePath -> T.Text -> FilePath -> Maybe SortBy
  -> (FilePath -> Char)
  -> Bool
  -> IO T.Text
listDirectoryAsGophermapWith hostname port serveRoot selectorPrefix requestedPath sortBy itemTypeFor allowDotfiles = do
    -- Hide dotfiles from auto-generated listings unless the block has
    -- opted in. They're typically internal state (.gophermap, .git/,
    -- .env) or transient SFTP/gvfs droppings (.giosaveXXXXX). The
    -- corresponding direct-access refusal lives in 'serveDirectoryWith'
    -- so an attacker who guesses the filename still can't fetch it.
    rawFiles <- listDirectory requestedPath
    let files = if allowDotfiles
                  then rawFiles
                  else filter (not . isDotFile) rawFiles
    fileInfos <- mapM (getFileInfo requestedPath) files

    let sortCriteria = fromMaybe ByName sortBy
        sortedFiles = sortFileInfo sortCriteria fileInfos
        relativePath = makeRelative serveRoot requestedPath
        parentSelector = buildEntrySelector selectorPrefix (takeDirectory relativePath) ""
        readmePath = requestedPath </> "README.txt"

    putStrLn $ "Listing directory: " ++ T.unpack parentSelector

    -- Check if README.txt exists and read it. When it does, we also
    -- record its on-disk size so the preview header can label the
    -- link with a human-readable byte count, and we filter the file
    -- out of the regular listing below so it isn't shown twice.
    readmeExists <- doesFileExist readmePath
    readmeContents <- if readmeExists
                      then T.lines <$> T.readFile readmePath
                      else return []
    readmeSize <- if readmeExists then getFileSize readmePath else pure 0

    -- Convert README contents to info items
    let readmeItems     = map info readmeContents
        readmeSizeText  = T.pack (formatFileSize readmeSize)
        readmeLabel     = "README.txt (" <> readmeSizeText <> "):"

    -- When the README is rendered as a preview, exclude it from the
    -- regular file listing — it's already visible above. Filter case-
    -- sensitively on "README.txt" (the only name the preview path
    -- recognises) so other similarly-named files aren't dropped.
    let isReadme fi    = fi.fiName == "README.txt"
        listingFiles   = if readmeExists
                           then filter (not . isReadme) sortedFiles
                           else sortedFiles

    -- "Directory listing for: ." reads as nonsense to a human at the
    -- root of a [[files]] block. Show the block's configured selector
    -- (or "/" for the catch-all "" block) instead, while keeping the
    -- relative-path display correct in subdirectories.
    let displayPath = case relativePath of
                        "." -> if T.null selectorPrefix
                                 then "/"
                                 else T.unpack selectorPrefix
                        p   -> p

    gopherItems <-
        mapM (\fi -> do
            let
              filename = T.unpack fi.fiName
              selector = buildEntrySelector selectorPrefix relativePath filename
              itemType = if fi.fiIsDir then '1' else itemTypeFor filename
              infoText = formatFileInfoTable fi
            return $ item itemType infoText selector hostname port
            ) listingFiles

    -- Compile the final menu, including README.txt contents if it exists
    return . render $
      info ("Directory listing for: " <> T.pack displayPath) :
      (if relativePath == "." then mempty else directory "Parent directory (..)" parentSelector hostname port) :
      info "" :
      (if null readmeContents then mempty else text readmeLabel (buildEntrySelector selectorPrefix relativePath "README.txt") hostname port : readmeItems ++ [info ""]) ++
      gopherItems

-- | Predicate: filename starts with @.@ — i.e. a Unix-style dotfile.
-- Used by 'listDirectoryAsGophermapWith' to filter hidden state out of
-- auto-generated listings.
isDotFile :: FilePath -> Bool
isDotFile ('.' : _) = True
isDotFile _         = False

-- | Build a gopher selector for an entry in an auto-generated
-- directory listing.
--
-- Combines the mount-point selector prefix, the subdirectory relative
-- to the served root, and the entry name into a single absolute-style
-- gopher selector:
--
--   * leading slash guaranteed (so the catch-all @selector = \"\"@
--     case doesn't emit @\"catalog\"@-style bare names);
--   * the @\".\"@ pseudo-segment that 'makeRelative' returns when
--     @requestedPath@ equals @serveRoot@ is stripped (so listing the
--     served root produces @\"\/catalog\"@, not @\"\/.\/catalog\"@);
--   * trailing slashes on the prefix are dropped before joining so
--     a configured @selector = \"\/foo\/\"@ doesn't yield
--     @\"\/foo\/\/bar\"@.
--
-- Pass an empty string for the third argument to compute the
-- selector for the directory itself rather than for an entry within it.
buildEntrySelector :: T.Text -> FilePath -> FilePath -> T.Text
buildEntrySelector prefix relPath name =
  let prefix' = dropWhileEnd (== '/') (T.unpack prefix)
      parts   = filter (\s -> not (null s) && s /= ".")
                       [prefix', relPath, name]
      joined  = intercalate "/" parts
  in T.pack $ if "/" `isPrefixOf` joined then joined else '/' : joined

-- | Serve a directory listing or file content. Equivalent to
-- @'serveDirectoryWith' … (\\_ -> pure Nothing) 'fileExtensionToItemType' False ".gophermap"@
-- (dotfiles refused by default; @.gophermap@ is the directory-menu source).
serveDirectory :: T.Text -> Int -> FilePath -> T.Text -> T.Text -> Maybe SortBy -> IO Response
serveDirectory host port root selectorRoot requestedPath sortBy =
  serveDirectoryWith host port root selectorRoot requestedPath sortBy
    (\_ -> pure Nothing) fileExtensionToItemType False ".gophermap"

-- | Serve a directory listing or file content, with two extension points:
--
-- * @fileHook@: called when the request resolves to a regular file. Returning
--   'Just' a 'Response' short-circuits the default 'FileResponse'; returning
--   'Nothing' falls through to serving the file verbatim. Use this to
--   intercept files of certain extensions and execute them as scripts (see
--   'Venusia.Routes' for the TOML-driven configuration).
-- * @itemTypeFor@: decides the gopher item-type character used for each file
--   in the auto-generated directory listing. Override the hardcoded
--   'fileExtensionToItemType' to type script-extension files as text/menu
--   instead of the default '9' (binary).
--
-- Path canonicalisation and the directory-traversal guard run before either
-- hook is consulted, so neither callback can be invoked with a path outside
-- the served root.
serveDirectoryWith
  :: T.Text                              -- ^ host
  -> Int                                 -- ^ port
  -> FilePath                            -- ^ served root
  -> T.Text                              -- ^ selector prefix
  -> T.Text                              -- ^ requested sub-path
  -> Maybe SortBy
  -> (FilePath -> IO (Maybe Response))   -- ^ file hook
  -> (FilePath -> Char)                  -- ^ item-type override fn
  -> Bool                                -- ^ allow dotfiles (listing + direct access)
  -> FilePath                            -- ^ directory-menu filename (default @.gophermap@)
  -> IO Response
serveDirectoryWith host port root selectorRoot requestedPath sortBy fileHook itemTypeFor allowDotfiles indexFileRaw = do
  let
    path = if T.isPrefixOf "/" requestedPath
           then root </> T.unpack (T.drop 1 requestedPath)
           else root </> T.unpack requestedPath
  -- Strip any path components from the configured indexFile so a
  -- footgun like `index_file = "../etc/passwd"` can't be used to read
  -- outside the served root via the directory-menu lookup. takeFileName
  -- returns just the final path component (or "" if there isn't one);
  -- both internal reads and the dotfile-exemption use this sanitised
  -- form, so the value is treated consistently.
  let indexFile = takeFileName indexFileRaw
  -- Security: ensure the canonicalised path is at or under the canonical
  -- root. Compare on path components, not raw strings — '/var/gopher' is
  -- a string-prefix of '/var/gopher2/secret' but is not a path ancestor
  -- of it.
  rootCanonical <- canonicalizePath root
  pathCanonical <- canonicalizePath path
  let indexFilePath = pathCanonical </> indexFile
  let isUnderRoot = splitDirectories rootCanonical
                      `isPrefixOf` splitDirectories pathCanonical
  putStrLn $ rootCanonical ++ " ... " ++ pathCanonical ++ show isUnderRoot
  if not isUnderRoot
    then return $ TextResponse $ error' "Access denied: Path is outside the root directory."
  -- Defence-in-depth: refuse any direct request whose client-supplied
  -- path traverses a dotfile component, unless the block has explicitly
  -- opted into dotfile exposure. Hiding from the listing alone isn't
  -- safety — an attacker who guesses /foo/.env should not be served.
  -- The configured 'indexFile' is exempt in the final-component position
  -- so a request for /foo/.gophermap (or whatever the block names its
  -- directory-menu source) still works. Internal reads of indexFile to
  -- render a non-dotfile selector aren't affected — this check is on
  -- the client-supplied path, not on what the server reads.
  else if not allowDotfiles && hasDisallowedDotfile rootCanonical pathCanonical indexFile
    then return $ TextResponse $ error' "Access denied: dotfile paths are disabled for this server block."
    else do
      -- Check if the index (directory-menu) file exists
      indexExists <- doesFileExist indexFilePath
      directoryExists <- doesDirectoryExist path
      if indexExists
        then do
          -- Read the index file and serve its content as a gophermap
          content <- T.readFile indexFilePath
          return . TextResponse $ gophermapRender host port content
        else if directoryExists then
          -- Generate directory listing with file info
          TextResponse <$> listDirectoryAsGophermapWith host port root selectorRoot pathCanonical sortBy itemTypeFor allowDotfiles
        else do
          -- File branch: consult the hook before falling back to FileResponse.
          mResp <- fileHook pathCanonical
          pure $ fromMaybe (FileResponse pathCanonical) mResp

-- | True when a client-supplied path traverses a dotfile component
-- that isn't the configured directory-menu index file in the
-- final-component position.
--
-- * Intermediate dotfile components are always disallowed (you don't
--   browse /through/ a dotfile).
-- * Final dotfile components are disallowed /unless/ they equal the
--   block's configured @indexFile@ — that one specific dotfile name
--   is the framework's directory-menu source, named-by-convention
--   even when the convention is a dotfile.
hasDisallowedDotfile :: FilePath -> FilePath -> FilePath -> Bool
hasDisallowedDotfile rootCanonical pathCanonical indexFile =
  let rootDirs = splitDirectories rootCanonical
      pathDirs = splitDirectories pathCanonical
      relDirs  = drop (length rootDirs) pathDirs
  in walk relDirs
  where
    -- Total recursion (no `init`/`last`): every non-final segment that
    -- is a dotfile is disallowed unconditionally; the final segment is
    -- disallowed only when it's a dotfile that isn't the configured
    -- index file.
    walk []       = False
    walk [s]      = isDotFile s && s /= indexFile
    walk (s : ss) = isDotFile s || walk ss