module Runner where

import Read 
import System.Environment

run :: (Show a) => ([String] -> a) -> IO String
run lineFn = do
  args <- getArgs
  lines <- readLines (head args)
  return (show . lineFn $ lines)


runWithInts :: ([Int] -> Int) -> IO String
runWithInts intFn = do
  result <- run (intFn . map read)
  return result
