module Advent2021.Day1 (main) where

import Lib.Runner

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)

windowSum :: [Int] -> Int
windowSum xs = sum (take 3 xs)

countIncreasesWindow :: [Int] -> Int
countIncreasesWindow [] = 0
countIncreasesWindow xz@(_:xs)
  | length xz < 3 = 0 
  | otherwise = (if windowSum xz < windowSum xs then 1 else 0) + countIncreasesWindow xs

main :: FilePath -> IO ()
main filePath = do
  singleResult <- runWithInts filePath countIncreases
  putStrLn ("Single increases: " ++ singleResult)
  windowResult <- runWithInts filePath countIncreasesWindow
  putStrLn ("Window increases: " ++ windowResult)
