module Advent2022.Day16.Part1
  ( run,
  )
where

import Advent2022.Day16.Base

maxPressureFromLines :: [String] -> Int
maxPressureFromLines = uncurry (maxPressure 30) . valveMapAndStartIndexFromLines

run :: [String] -> String
run ss = "Result: " ++ show (maxPressureFromLines ss)
