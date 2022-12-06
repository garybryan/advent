module Advent2022.Day06.Part1 (indexAfterMarker) where

import Advent2022.Day06.Base

indexAfterMarker :: (Eq a) => [a] -> Int
indexAfterMarker = indexAfterUniquePrefix 4
