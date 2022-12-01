module Advent2021.Day1 (main) where

import Lib.Read

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)

windowSum :: Int -> [Int] -> Int
windowSum k xs = sum (take k xs)

-- Could this be optimised with prefix sums?
countIncreasesWindow :: Int -> [Int] -> Int
countIncreasesWindow _ [] = 0
countIncreasesWindow k xz@(_:xs)
  | length xz < k = 0
  | otherwise = (if windowSum k xz < windowSum k xs then 1 else 0) + countIncreasesWindow k xs

main :: FilePath -> IO ()
main filePath = do
  counts <- readInts filePath

  let singleResult = countIncreases counts
  putStrLn ("Single increases: " ++ show singleResult)

  let windowResult = countIncreasesWindow 3 counts
  putStrLn ("Window increases: " ++ show windowResult)
