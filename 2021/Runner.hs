module Runner where

import Read 
import System.Environment

run :: ([String] -> String) -> IO String
run lineFn = do
  args <- getArgs
  lines <- readLines (head args)
  return (lineFn lines)

runWithInts :: ([Int] -> Int) -> IO String
runWithInts intFn = do
  result <- run (show . intFn . map read)
  return result
