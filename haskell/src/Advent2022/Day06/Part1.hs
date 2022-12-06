module Advent2022.Day06.Part1 (indexAfterMarker, run) where

import Advent2022.Day06.Base

indexAfterMarker :: (Ord a) => [a] -> Int
indexAfterMarker = indexAfterUniquePrefix 4

run :: [String] -> String
run ss = "Index after first start-of-packet marker: " ++ show (indexAfterMarker $ parseLines ss)
