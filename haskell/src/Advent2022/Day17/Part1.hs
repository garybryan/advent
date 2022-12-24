module Advent2022.Day17.Part1
  ( run,
  )
where

import Advent2022.Day17.Base

run :: [String] -> String
run ss = "Height after 2022 moves: " ++ show (heightAfterBlocks 2022 jetMoves)
  where
    jetMoves = parseLine $ head ss
