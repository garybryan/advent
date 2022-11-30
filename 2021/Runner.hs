module Runner where

import Read 
import System.Environment

run :: ([Int] -> String) -> IO ()
run intsFn = do
  args <- getArgs
  ints <- readInts . head $ args
  putStrLn . intsFn $ ints
