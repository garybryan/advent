module Advent2022.Day12.Part1
  ( bfsLines,
    run,
  )
where

import Advent2022.Day12.Base

bfsLines :: [String] -> Int
bfsLines ss = bfsFrom start end hs
  where
    (hs, start, end) = parseLines ss

run :: [String] -> String
run ss = "Result: " ++ show (bfsLines ss)
