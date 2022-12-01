module Advent2021.Day1 (run) where

import Lib.Read (readInts)

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)

windowSum :: Int -> [Int] -> Int
windowSum k = sum . take k

countIncreasesWindow :: Int -> [Int] -> Int
countIncreasesWindow _ [] = 0
countIncreasesWindow k xz@(_:xs)
  | length xz < k = 0
  | otherwise = (if windowSum k xz < windowSum k xs then 1 else 0) + countIncreasesWindow k xs

run :: FilePath -> IO ()
run filePath = do
  counts <- readInts filePath

  let singleResult = countIncreases counts
  putStrLn ("Single increases: " ++ show singleResult)

  let windowResult = countIncreasesWindow 3 counts
  putStrLn ("Window increases: " ++ show windowResult)
