module Advent2022.Day17.Part2
  ( run,
  )
where

import Advent2022.Day17.Base

run :: [String] -> String
run ss = "Height after 10^12 moves: " ++ show (heightAfterBlocks 1000000000000 jetMoves)
  where
    jetMoves = parseLine $ head ss
