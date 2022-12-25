module Advent2022.Day18.Part1
  ( run,
  )
where

import Advent2022.Day18.Base

surfaceAreaFromLines :: [String] -> Int
surfaceAreaFromLines = surfaceArea . parseLines

run :: [String] -> String
run ss = "Result: " ++ show (surfaceAreaFromLines ss)
