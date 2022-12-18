module Lib.Run (runOnFile) where

import Data.Time
import Lib.Read (readLines)

runOnFile :: ([String] -> String) -> FilePath -> IO ()
runOnFile run filePath = do
  start <- getCurrentTime
  fileLines <- readLines filePath
  putStrLn $ run fileLines
  end <- getCurrentTime
  putStrLn $ "Running time: " ++ show (diffUTCTime end start)
