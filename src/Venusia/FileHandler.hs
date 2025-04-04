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
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import Venusia.MenuBuilder (gophermapRender, render, item)
import Venusia.Server (Request(..), Response(..))
import System.FilePath ((</>), takeExtension, makeRelative)
import qualified Data.ByteString as BL

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

listDirectoryAsGophermap :: T.Text -> Int -> FilePath -> T.Text -> FilePath -> IO T.Text
listDirectoryAsGophermap hostname port serveRoot selectorPrefix requestedPath = do
    files <- listDirectory requestedPath
    -- If an item is a directory, use `directory` for the item type, otherwise use `fileExtensionToItemType`
    gopherItems <-
        mapM (\dirOrFile -> do
            let fullPath = requestedPath </> dirOrFile
            isDir <- doesDirectoryExist fullPath
            let
              relativePath = makeRelative serveRoot requestedPath
              selector = T.pack $ T.unpack selectorPrefix </> relativePath </> dirOrFile
              itemType = if isDir then '1' else fileExtensionToItemType $ T.unpack selector
            return $ item itemType (T.pack dirOrFile) (selector) hostname port
            ) files
    return . render $ gopherItems


serveDirectory :: T.Text -> Int -> FilePath -> T.Text -> T.Text -> IO Response
serveDirectory host port root selectorRoot requestedPath = do
  let
    path = if T.isPrefixOf "/" requestedPath
           then root </> T.unpack (T.drop 1 requestedPath)
           else root </> T.unpack requestedPath
    gopherMapPath = path </> ".gophermap"
-- Check if the .gophermap file exists
  gopherMapExists <- doesFileExist gopherMapPath
  directoryExists <- doesDirectoryExist path
  if gopherMapExists
    then do
      -- Read the .gophermap file and serve its content
      content <- T.readFile gopherMapPath
      return . TextResponse $ gophermapRender host port content
    else if directoryExists then
      -- Is it 
      TextResponse <$> listDirectoryAsGophermap host port root selectorRoot path
    else do
      -- just serve the file
      BinaryResponse <$> BL.readFile (root </> T.unpack requestedPath)