module Advent2022.Day4.Base (numMatchingRanges, lineRanges) where

import Data.List.Split

type Range = (Int, Int)

numMatchingRanges :: (Range -> Range -> Bool) -> [(Range, Range)] -> Int
numMatchingRanges fn = length . filter (uncurry fn)

parseRange :: String -> Range
parseRange s = (read $ head nums, read $ last nums)
  where
    nums = splitOn "-" s

lineRanges :: String -> (Range, Range)
lineRanges l = (head ranges, last ranges)
  where
    ranges = map parseRange $ splitOn "," l
