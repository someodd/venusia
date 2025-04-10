{- | Gopher Gateway/connect to processes.

Allow a request to be sent to a process and return the response.
Supports executing processes with or without arguments.
Includes proper error handling for process execution.

-}

module Venusia.Gateway 
  ( executeProcess
  , executeProcessWithArgs
  ) where

import Venusia.Server (Response(..))
import qualified System.Process as P
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Venusia.MenuBuilder (error', info)

-- | Execute a process without arguments and return the response.
executeProcess :: String -> IO Response
executeProcess command = do
    result <- try (P.readProcess command [] "") :: IO (Either SomeException String)
    case result of
        Left err  -> return $ TextResponse $ T.pack $ "Process execution failed: " ++ show err
        Right out -> return $ TextResponse $ T.pack out

-- | Execute a process with arguments and return the response.
--
-- You NEED to use `asMenu` for a proper response to search queries, I believe this is
-- part of the gopher spec, or at least the client I tested expects a menu as a search
-- response.
executeProcessWithArgs :: String -> [String] -> Bool -> IO Response
executeProcessWithArgs command args asMenu = do
    result <- try (P.readProcess command args "") :: IO (Either SomeException String)
    case result of
        Left err  ->
            if asMenu
                then return $ TextResponse $ error' . T.pack $ "Process execution failed: " ++ show err
                else return $ TextResponse $ T.pack $ "Process execution failed: " ++ show err
        Right out ->
            if asMenu
                then
                    return $ TextResponse . T.unlines $ info . T.pack <$> lines out
                else
                    return $ TextResponse $ T.pack out