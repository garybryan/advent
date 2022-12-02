module Advent2021.Day1 (run) where

import qualified Data.Vector as V

import Lib.Read (readInts)

-- TODO use fold?
countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [_] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)

-- Prefix sum optimisation to allow finding the sum for an arbitrary window in constant time,
-- therefore making the whole window algorithm O(n) rather than O(n*k) for any k.

prefixSums :: [Int] -> V.Vector Int
prefixSums = V.fromList . scanl (+) 0

windowSum :: Int -> V.Vector Int -> Int -> Int
windowSum k ps i = ps V.! (i + k) - ps V.! i

windowSums :: Int -> V.Vector Int -> [Int]
windowSums k ps = map (windowSum k ps) [0..length ps - k - 1]

countIncreasesWindow :: Int -> [Int] -> Int
countIncreasesWindow k = countIncreases . windowSums k . prefixSums

run :: FilePath -> IO ()
run filePath = do
  counts <- readInts filePath

  let singleResult = countIncreases counts
  putStrLn ("Single increases: " ++ show singleResult)

  let windowResult = countIncreasesWindow 3 counts
  putStrLn ("Window increases: " ++ show windowResult)
