module Advent2022.Day14.Part2
  ( sandFallToSource,
    sandFallFromLines,
    run,
  )
where

import Advent2022.Day14.Base
import qualified Data.Set as Set

sandFallToSource :: Int -> Point -> Points -> Int
sandFallToSource maxY source initialPs = fall initialPs 0 source
  where
    fall :: Points -> Int -> Point -> Int
    fall ps unit p@(x, y)
      | y == maxY + 1 = nextUnit
      | (x, y + 1) `Set.notMember` ps = fall ps unit (x, y + 1)
      | (x - 1, y + 1) `Set.notMember` ps = fall ps unit (x - 1, y + 1)
      | (x + 1, y + 1) `Set.notMember` ps = fall ps unit (x + 1, y + 1)
      | p == source = unit + 1
      | otherwise = nextUnit
      where
        nextUnit = fall (Set.insert p ps) (unit + 1) source

sandFallFrom500 :: Points -> Int
sandFallFrom500 ps = sandFallToSource (maxYOfPoints ps) source ps
  where
    source = (500, 0)

sandFallFromLines :: [String] -> Int
sandFallFromLines = sandFallFrom500 . pointsOnAllLines . map parseLine

run :: [String] -> String
run ss = "Number of units of sand settled before source blocked: " ++ show (sandFallFromLines ss)
