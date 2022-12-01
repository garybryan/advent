module Utils.Runner where

import Utils.Read 
import System.Environment

run :: ([String] -> a) -> IO a
run linesFn = do
  args <- getArgs
  lines <- readLines (head args)
  return (linesFn lines)


runWithInts :: ([Int] -> Int) -> IO String
runWithInts intsFn = do
  result <- run(intsFn . map read)
  return (show result)
