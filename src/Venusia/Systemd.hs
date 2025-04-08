module Venusia.Systemd (
  -- * Service setup function
    setupSystemdService
) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Process (callProcess)
import System.Exit (exitSuccess)

-- | Set up a systemd service for a Venusia Gopher server on Debian
setupSystemdService :: 
     String   -- ^ Service name (e.g., "venusia-gopher")
  -> FilePath -- ^ Path to the executable
  -> String   -- ^ Port number
  -> Maybe String -- ^ User to run as (or Nothing for default)
  -> Maybe String -- ^ Group to run as (or Nothing for default)
  -> Maybe FilePath -- ^ Working directory (or Nothing for default)
  -> IO ()
setupSystemdService serviceName execPath port userMaybe groupMaybe workDirMaybe = do
  putStrLn "Setting up systemd service..."
  
  -- Create the service file content
  let serviceFile = T.unlines
        [ "[Unit]"
        , "Description=" <> T.pack serviceName <> " Gopher Server"
        , "After=network.target"
        , ""
        , "[Service]"
        , "Type=simple"
        , "ExecStart=" <> T.pack execPath
        , maybe "" (\dir -> "WorkingDirectory=" <> T.pack dir) workDirMaybe
        , maybe "" (\u -> "User=" <> T.pack u) userMaybe
        , maybe "" (\g -> "Group=" <> T.pack g) groupMaybe
        , "Restart=on-failure"
        , "StandardOutput=journal"
        , "StandardError=journal"
        , ""
        , "[Install]"
        , "WantedBy=multi-user.target"
        ]
  
  -- Write the service file directly to the systemd directory
  let serviceFilePath = "/etc/systemd/system/" <> serviceName <> ".service"
  
  -- Check if we have permission to write to /etc/systemd/system
  serviceFileExists <- doesFileExist serviceFilePath
  if serviceFileExists
    then putStrLn $ "Service file already exists at " ++ serviceFilePath ++ ". Please remove it first."
    else do
      -- Try to create the service file
      putStrLn $ "Creating service file at " ++ serviceFilePath
      putStrLn "This may require sudo privileges."
      
      -- Write to a temporary file first
      let tempFilePath = "/tmp/" ++ serviceName ++ ".service"
      TIO.writeFile tempFilePath serviceFile
      
      -- Use sudo to move it to the correct location
      callProcess "sudo" ["cp", tempFilePath, serviceFilePath]
      callProcess "sudo" ["chmod", "644", serviceFilePath]
      
      -- Reload systemd, enable and start the service
      putStrLn "Reloading systemd daemon..."
      callProcess "sudo" ["systemctl", "daemon-reload"]
      
      putStrLn $ "Enabling " ++ serviceName ++ " service..."
      callProcess "sudo" ["systemctl", "enable", serviceName]
      
      putStrLn $ "Starting " ++ serviceName ++ " service..."
      callProcess "sudo" ["systemctl", "start", serviceName]
      
      putStrLn "Service setup complete!"
      putStrLn $ "You can check the status with: sudo systemctl status " ++ serviceName
      
      -- Clean up
      callProcess "rm" [tempFilePath]