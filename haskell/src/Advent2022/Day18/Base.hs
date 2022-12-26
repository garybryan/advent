module Advent2022.Day18.Base
  ( Point,
    parseLine,
    parseLines,
  )
where

import Lib.Parsing (parseOrError, pointParser3d)

type Point = (Int, Int, Int)

parseLine :: String -> Point
parseLine = parseOrError pointParser3d

parseLines :: [String] -> [Point]
parseLines = map parseLine
