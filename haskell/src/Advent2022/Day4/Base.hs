module Advent2022.Day4.Base (Range, numMatchingRanges, parseLine) where

import Data.List.Split

type Range = (Int, Int)

numMatchingRanges :: (Range -> Range -> Bool) -> [(Range, Range)] -> Int
numMatchingRanges fn = length . filter (uncurry fn)

parseRange :: String -> Range
parseRange s = (read $ head ss, read $ last ss)
  where
    ss = splitOn "-" s

parseLine :: String -> (Range, Range)
parseLine l = (head rs, last rs)
  where
    rs = map parseRange $ splitOn "," l
