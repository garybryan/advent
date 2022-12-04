module Advent2022.Day4.Part2 (hasOverlap, numOverlap, run) where

import Advent2022.Day4.Base
import Lib.Read (readLines)

type Range = (Int, Int)

hasOverlap :: Range -> Range -> Bool
hasOverlap (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

numOverlap :: [(Range, Range)] -> Int
numOverlap = numMatchingRanges hasOverlap

numOverlapFromLines :: [String] -> Int
numOverlapFromLines = numOverlap . map parseLine

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  putStrLn $ "Number of pairs with overlapping ranges: " ++ show (numOverlapFromLines fileLines)
