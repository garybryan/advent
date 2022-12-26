module Lib.Run (runOnFile) where

import Data.Time
import Lib.Read (readLines)

runOnFile :: ([String] -> String) -> FilePath -> IO ()
runOnFile run filePath = do
  start <- getCurrentTime
  ranLines <- run <$> readLines filePath
  putStrLn ranLines
  end <- getCurrentTime
  putStrLn $ "Running time: " ++ show (diffUTCTime end start)
