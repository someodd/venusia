{- | A default handler which will serve a directory listing.

Has support for .gophermap files. If present in the directory, it will be displayed
instead of the directory listing.

Note that .gophermap files allow for shorthand where the host and port don't need to be
specified (assume it from server config).

-}

module Venusia.FileHandler
  ( serveDirectory
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
  ( listDirectory
  , doesFileExist
  , doesDirectoryExist
  , canonicalizePath
  , getModificationTime
  , pathIsSymbolicLink
  )
import Venusia.MenuBuilder (gophermapRender, render, item, error', info, menu, directory, text)
import Venusia.Server (Request(..), Response(..))
import System.FilePath ((</>), takeExtension, makeRelative, takeDirectory)
import qualified Data.ByteString as BL
import Data.List (isPrefixOf, sortBy, partition)
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
    (dirs, files) = partition fiIsDir fileInfos
    sortedDirs = case sortCriteria of
      ByName -> sortBy (comparing fiName) dirs
      BySize -> dirs  -- Directories all have size 0, so keep original order
      ByTime -> sortBy (comparing fiModTime) dirs
    sortedFiles = case sortCriteria of
      ByName -> sortBy (comparing fiName) files
      BySize -> sortBy (comparing fiSize) files
      ByTime -> sortBy (comparing fiModTime) files
  in
    sortedDirs ++ sortedFiles

-- Modify the formatFileInfoTable function to ensure proper spacing
formatFileInfoTable :: FileInfo -> T.Text
formatFileInfoTable fi =
  let
    -- Add "/" suffix to directory names
    name = if fiIsDir fi
           then fiName fi `T.append` T.pack "/"
           else fiName fi

    -- Truncate filename if too long and pad to fixed width
    displayName = truncateFilename name

    -- Format and pad the size column
    size = T.pack $ formatFileSize (fiSize fi)
    paddedSize = T.justifyRight sizeColumnWidth ' ' size

    -- Format date to YYYY-MM-DD only
    time = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (fiModTime fi)
  in
    displayName `T.append` paddedSize `T.append` T.pack "  " `T.append` time

-- Update the truncateFilename function to handle the new length
truncateFilename :: T.Text -> T.Text
truncateFilename name =
  if T.length name <= maxFilenameLength
  then name `T.append` T.replicate (maxFilenameLength - T.length name) (T.singleton ' ')
  else T.take (maxFilenameLength - 4) name `T.append` T.pack "... "

-- | List directory contents as a gophermap
listDirectoryAsGophermap :: T.Text -> Int -> FilePath -> T.Text -> FilePath -> Maybe SortBy -> IO T.Text
listDirectoryAsGophermap hostname port serveRoot selectorPrefix requestedPath sortBy = do
    files <- listDirectory requestedPath
    fileInfos <- mapM (getFileInfo requestedPath) files

    let sortCriteria = fromMaybe ByName sortBy
        sortedFiles = sortFileInfo sortCriteria fileInfos
        relativePath = makeRelative serveRoot requestedPath
        parentSelector = T.pack $ T.unpack selectorPrefix <> takeDirectory relativePath <> "/"
        readmePath = requestedPath </> "README.txt"

    putStrLn $ "Listing directory: " ++ T.unpack parentSelector
    
    -- Check if README.txt exists and read it
    readmeExists <- doesFileExist readmePath
    readmeContents <- if readmeExists
                      then T.lines <$> T.readFile readmePath
                      else return []
    
    -- Convert README contents to info items
    let readmeItems = map info readmeContents
        
    gopherItems <-
        mapM (\fi -> do
            let
              filename = T.unpack (fiName fi)
              selector = T.pack $ T.unpack selectorPrefix </> relativePath </> filename
              itemType = if fiIsDir fi then '1' else fileExtensionToItemType filename
              infoText = formatFileInfoTable fi
            return $ item itemType infoText selector hostname port
            ) sortedFiles
            
    -- Compile the final menu, including README.txt contents if it exists
    return . render $
      info ("Directory listing for: " <> T.pack relativePath) :
      (if relativePath == "." then mempty else directory "Parent directory (..)" parentSelector hostname port) :
      info "" :
      (if null readmeContents then mempty else text "README.txt:" (selectorPrefix <> "/README.txt") hostname port : readmeItems ++ [info ""]) ++
      gopherItems

-- | Serve a directory listing or file content
serveDirectory :: T.Text -> Int -> FilePath -> T.Text -> T.Text -> Maybe SortBy -> IO Response
serveDirectory host port root selectorRoot requestedPath sortBy = do
  let
    path = if T.isPrefixOf "/" requestedPath
           then root </> T.unpack (T.drop 1 requestedPath)
           else root </> T.unpack requestedPath
  -- check if the path is a subdirectory of the root
  -- (this is a security measure to prevent directory traversal attacks)
  rootCanonical <- canonicalizePath root
  pathCanonical <- canonicalizePath path
  let gopherMapPath = pathCanonical </> ".gophermap"
  let isSubdirectory = rootCanonical `isPrefixOf` pathCanonical
  putStrLn $ rootCanonical ++ " ... " ++ pathCanonical ++ show isSubdirectory
  if not isSubdirectory
    then return $ TextResponse $ error' "Access denied: Path is outside the root directory."
    else do
      -- Check if the .gophermap file exists
      gopherMapExists <- doesFileExist gopherMapPath
      directoryExists <- doesDirectoryExist path
      if gopherMapExists
        then do
          -- Read the .gophermap file and serve its content
          content <- T.readFile gopherMapPath
          return . TextResponse $ gophermapRender host port content
        else if directoryExists then
          -- Generate directory listing with file info
          TextResponse <$> listDirectoryAsGophermap host port root selectorRoot pathCanonical sortBy
        else do
          -- just serve the file
          BinaryResponse <$> BL.readFile pathCanonical