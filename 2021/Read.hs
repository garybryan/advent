module Read where

readInts :: FilePath -> IO [Int]
readInts path = do
  contents <- readFile path
  return (map read . lines $ contents)

