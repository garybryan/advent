module Advent2022.Day4.Base (contains, numContains, lineRanges, run) where

import Data.List.Split
import Lib.Read (readLines)

type Range = (Int, Int)

contains :: Range -> Range -> Bool
contains (s1, e1) (s2, e2)
  | s1 >= s2 && e1 <= e2 = True
  | s2 >= s1 && e2 <= e1 = True
  | otherwise = False

numContains :: [(Range, Range)] -> Int
numContains = sum . map (fromEnum . uncurry contains)

parseRange :: String -> Range
parseRange s = (read $ head nums, read $ last nums)
  where
    nums = splitOn "-" s

lineRanges :: String -> (Range, Range)
lineRanges l = (head ranges, last ranges)
  where
    ranges = map parseRange $ splitOn "," l

numContainsFromLines :: [String] -> Int
numContainsFromLines = numContains . map lineRanges

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  putStrLn $ "Number of pairs with containing ranges: " ++ show (numContainsFromLines fileLines)
