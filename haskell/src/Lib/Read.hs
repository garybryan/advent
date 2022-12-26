module Lib.Read (readLines) where

readLines :: FilePath -> IO [String]
readLines path = lines <$> readFile path
