module Read where

readLines :: FilePath -> IO [String]
readLines path = do
  contents <- readFile path
  return (lines contents)

