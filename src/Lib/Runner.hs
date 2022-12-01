module Lib.Runner (run, runWithInts) where

import Lib.Read 

run :: FilePath -> ([String] -> a) -> IO a
run filePath linesFn = do
  fileLines <- readLines filePath
  return (linesFn fileLines)


runWithInts :: FilePath -> ([Int] -> Int) -> IO String
runWithInts filePath intsFn = do
  result <- run filePath (intsFn . map read)
  return (show result)
