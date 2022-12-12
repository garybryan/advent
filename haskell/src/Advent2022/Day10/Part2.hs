module Advent2022.Day10.Part2
  ( crtChar,
    run,
  )
where

import Advent2022.Day10.Base
import Data.List (intercalate)
import Data.List.Split (chunksOf)

-- Render a light pixel if the tick number for the row is within a 3-pixel
-- range with the value at the centre, or in other words, the difference
-- between the tick number and the centre is 1 or less.
-- This is a bit awkward since pixel numbers are 0-indexed, so the "first
-- pixel" is pixel zero. Getting the difference between the zero-based tick
-- (tick - 1) and the one-based value compensates for this.
crtChar :: Int -> (Int, Int) -> Char
crtChar rowWidth (t, v)
  | abs (((t - 1) `mod` rowWidth) - v) <= 1 = '#'
  | otherwise = '.'

crtRows :: Int -> [(Int, Int)] -> [String]
crtRows rowWidth tvs = chunksOf rowWidth $ map (crtChar rowWidth) tvs

crtFromInstrs :: Int -> [Instr] -> [String]
crtFromInstrs rowWidth is = crtRows rowWidth (valuesWithTicks is)

crtRowsFromLines :: Int -> [String] -> [String]
crtRowsFromLines rowWidth ss = crtFromInstrs rowWidth (map parseLine ss)

run :: [String] -> String
run ss = "Result:\n" ++ intercalate "\n" (crtRowsFromLines 40 ss)
