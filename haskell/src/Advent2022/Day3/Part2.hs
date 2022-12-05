module Advent2022.Day3.Part2 (priorityGroupsOf3, run) where

import Advent2022.Day3.Base
import Data.List.Split

priorityGroupsOf3 :: [String] -> Int
priorityGroupsOf3 = sum . map (priority . commonItemAll) . chunksOf 3

run :: [String] -> String
run ls = "Total priority of common items in groups of 3: " ++ show (priorityGroupsOf3 ls)
