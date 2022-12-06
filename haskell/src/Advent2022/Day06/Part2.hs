module Advent2022.Day06.Part2 (indexAfterMarker, run) where

import Advent2022.Day06.Base

indexAfterMarker :: (Ord a) => [a] -> Int
indexAfterMarker = indexAfterUniquePrefix 14

run :: [String] -> String
run ss = "Index after first start-of-message marker: " ++ show (indexAfterMarker $ parseLines ss)
