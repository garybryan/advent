module Advent2022.Day09.Part1 (run) where

import Advent2022.Day09.Base

run :: [String] -> String
run ss = "Number of points visited by tail: " ++ show (numTailVisited 2 ss)
