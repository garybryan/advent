module Runner where

import Read 
import System.Environment

runWithInts :: ([Int] -> String) -> IO String
runWithInts intsFn = do
  args <- getArgs
  ints <- readInts . head $ args
  return . intsFn $ ints
