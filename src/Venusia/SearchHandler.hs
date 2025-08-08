{- |
Module      : Venusia.SearchHandler
Description : Handles Gopher search requests.

This module provides functionality to handle search queries (Gopher item type 7),
returning a Gopher menu with the search results.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Venusia.SearchHandler
    ( serveSearch
    ) where

import           Control.Exception          (try, SomeException)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           System.FilePath            ((</>), makeRelative, FilePath)

import           Ryvm.Search                (SearchResult (..), getSearchResults)
import           Venusia.MenuBuilder
import           Venusia.Server             (Request (..), Response (..))

-- | Determines if a file's content represents a Gophermap.
--
-- This is a heuristic check based on the presence of tab-separated values.
-- It's efficient as it avoids reading the entire file into memory at once.
isGophermapFile :: T.Text -> Bool
isGophermapFile content =
    let
        -- Consider only non-empty lines that are not just whitespace or a period.
        isDataLine = not . T.null . T.strip
        relevantLines = filter isDataLine . T.lines $ content
        -- A valid gophermap line typically has 3 tabs (4 fields).
        isValidLine line = T.count "\t" line == 3
    in not (null relevantLines) && all isValidLine relevantLines

-- | Constructs a Gopher menu response from search results.
buildSearchResponse :: T.Text -> FilePath -> T.Text -> Int -> [SearchResult] -> IO Response
buildSearchResponse query targetPath host port results = do
    menuItems <- concat <$> mapM (searchResultToMenuItem targetPath host port) results
    let header = info ("Results for search: " <> query)
    pure . TextResponse . render $ header : menuItems

-- | Converts a single search result into a Gopher menu item.
--
-- Safely reads file content to determine the item type.
searchResultToMenuItem :: FilePath -> T.Text -> Int -> SearchResult -> IO [T.Text]
searchResultToMenuItem targetPath host port SearchResult{..} = do
    -- The path from Ryvm should be absolute, so we make it relative to our target.
    let relativePathToFile = makeRelative targetPath filePath
        pathInSource       = targetPath </> relativePathToFile

    -- Safely read the file content to determine its type.
    eContents <- try (loadFileContent pathInSource) :: IO (Either SomeException T.Text)

    let (itemType, selector) = case eContents of
            Right contents | isGophermapFile contents -> ('1', T.pack $ "/" <> relativePathToFile)
            _ -> ('0', T.pack $ "/" <> relativePathToFile)

    let title = item itemType (T.pack highlightedFilePath) selector host port

    pure
        [ info ""
        , title
        , info $ "Rank score: " <> T.pack (show score)
        , info contexts
        ]

-- | Reads a file's content.
loadFileContent :: FilePath -> IO T.Text
loadFileContent = TIO.readFile

-- | Serves a search results page.
--
-- This is the core function for handling a search request. It orchestrates
-- fetching search results and building the Gopher response.
serveSearch :: FilePath -> T.Text -> Int -> T.Text -> IO Response
serveSearch targetPath host port query = do
    -- We assume getSearchResults can handle an empty query.
    -- The `filterGophermaps` function is applied to the content before indexing.
    results <- getSearchResults (Just filterGophermaps) query targetPath targetPath
    buildSearchResponse query targetPath host port results

-- | A pre-processing filter for content before it's indexed.
-- Currently, it's an identity function but can be expanded.
filterGophermaps :: T.Text -> T.Text
filterGophermaps = id

-- | The main handler for search requests (Gopher item type 7).
--
-- It extracts the query from the request and passes it to `serveSearch`.
handleSearch :: FilePath -> T.Text -> Int -> Request -> IO Response
handleSearch targetPath host port request = do
    let query = fromMaybe "" request.reqQuery
    serveSearch targetPath host port query