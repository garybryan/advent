module Advent2022.Day07.Part1 (namesAndSizesUnder100k, totalUnder100k, run) where

import Advent2022.Day07.Base

namesAndSizesUnder100k :: FS -> [(String, Int)]
namesAndSizesUnder100k fs = dirsWithSize (<= 100000) (fsMap fs) 0

totalUnder100k :: [FSKey] -> [String] -> Int
totalUnder100k nks ss = sum $ map snd (namesAndSizesUnder100k fs)
  where
    fs = fsFromCommands nks (parseLines ss)

run :: [String] -> String
run ss = "Total size of directories with sizes up to 100000: " ++ show (totalUnder100k [1 ..] ss)
