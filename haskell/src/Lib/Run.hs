module Lib.Run (runOnFile) where

import Lib.Read (readLines)

runOnFile :: ([String] -> String) -> FilePath -> IO ()
runOnFile run filePath = do
  fileLines <- readLines filePath
  putStrLn $ run fileLines
