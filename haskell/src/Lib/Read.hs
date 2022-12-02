module Lib.Read (readLines, readInts) where

readLines :: FilePath -> IO [String]
readLines path = do
  contents <- readFile path
  return $ lines contents

readInts :: FilePath -> IO [Int]
readInts path = do
  fileLines <- readLines path
  return $ map read fileLines
