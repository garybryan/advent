module Advent2022.Day15.Part2
  ( distressBeaconPos,
    tuningFreq,
    run,
  )
where

import Advent2022.Day15.Base
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Lib.Types (Point)

{-
  Interval merge solution similar to part 1:

  Merge the ranges on each row that are covered by sensors. There will only be
  one row with a gap in ranges, and that gap will be the x position of the
  distress beacon. As soon as we find that, we're done.

  This won't work in the "edge" case where the sensor is at a horizontal edge
  of the graph (see what I did there). Luckily, this isn't the case for the
  supplied data, so I've not handled it, but it would just require checking if
  there is a first range starting above the lower bound or a last range below
  the upper bound.

  This solution isn't great as it scales with the rows: O(R * S log S) for R
  rows and S sensors. But it gives a result in 8 seconds, unlike my previous
  one that looked like it would take hours.

  There's surely a solution based on geometry that scales with just S and will
  run in a few milliseconds, but I might come back to that later now that I
  have something that works.
-}

rangeGap :: [Range] -> Maybe Int
rangeGap rs = gap (head sortedRs) (tail sortedRs)
  where
    sortedRs = sort rs
    gap :: Range -> [Range] -> Maybe Int
    gap _ [] = Nothing
    gap (s, e) ((is, ie) : rest)
      | is > e = Just (e + 1)
      | otherwise = gap (s, max e ie) rest

distressBeaconPos :: (Int, Int) -> [(Point, Point)] -> Point
distressBeaconPos (xyMin, xyMax) ssAndBs = head $ mapMaybe gapPos rangeGaps
  where
    ssAndRs = sensorsAndRadii ssAndBs
    rangeGaps = [(rangeGap (rowSensorRanges y ssAndRs), y) | y <- [xyMin .. xyMax]]
    gapPos (Nothing, _) = Nothing
    gapPos (Just x, y) = Just (x, y)

tuningFreq :: Int -> Point -> Int
tuningFreq mult (x, y) = x * mult + y

distressBeaconTuningFreq :: (Int, Int) -> Int -> [(Point, Point)] -> Int
distressBeaconTuningFreq bounds mult = tuningFreq mult . distressBeaconPos bounds

resultFromLines :: [String] -> Int
resultFromLines = distressBeaconTuningFreq (0, 4000000) 4000000 . map parseLine

run :: [String] -> String
run ss = "Distress beacon tuning frequency: " ++ show (resultFromLines ss)
