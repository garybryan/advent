module Advent2022.Day04.Part1 (hasContainment, numContain, run) where

import Advent2022.Day04.Base

hasContainment :: Range -> Range -> Bool
hasContainment (s1, e1) (s2, e2) = s1 >= s2 && e1 <= e2 || s2 >= s1 && e2 <= e1

numContain :: [(Range, Range)] -> Int
numContain = numMatchingRanges hasContainment

numContainFromLines :: [String] -> Int
numContainFromLines = numContain . map parseLine

run :: [String] -> String
run ls = "Number of pairs with containing ranges: " ++ show (numContainFromLines ls)
