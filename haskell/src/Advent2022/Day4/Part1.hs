module Advent2022.Day4.Part1 (contain, numContain, run) where

import Advent2022.Day4.Base
import Lib.Read (readLines)

type Range = (Int, Int)

contain :: Range -> Range -> Bool
contain (s1, e1) (s2, e2) = s1 >= s2 && e1 <= e2 || s2 >= s1 && e2 <= e1

numContain :: [(Range, Range)] -> Int
numContain = numMatchingRanges contain

numContainFromLines :: [String] -> Int
numContainFromLines = numContain . map lineRanges

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  putStrLn $ "Number of pairs with containing ranges: " ++ show (numContainFromLines fileLines)
