module Advent2022.Day15.Part1
  ( rowSensorRanges,
    rangeTotal,
    nRowPointsNoBeacon,
    run,
  )
where

import Advent2022.Day15.Base
import Data.List (sort)
import qualified Data.Set as Set
import Lib.Types (Point)

{-
  For the row, find all of the ranges of x-coordinates covered by a beacon,
  and merge overlapping ranges. The total of the range lengths is the number
  of columns in the row covered by a beacon. Then just subtract the number
  of the these columns that actually contain a beacon to get the number that
  cannot contain one.

  This is based on the textbook interval-merge algorithm: sort the ranges by
  start, then join together overlapping ones by maintain a current range,
  iterating through the other ranges, updating the current one to end on the
  next one's end point if the latter is higher and they overlap (next start
  comes before current end), and closing the current range if the next one
  doesn't overlap.

  The difference is just that this returns the sum of the merged ranges'
  lengths, not the merged ranges themselves, as that is the number of points in
  the row covered by beacons.
-}

rangeTotal :: [Range] -> Int
rangeTotal rs = total (head sortedRs) (tail sortedRs)
  where
    sortedRs = sort rs
    total :: Range -> [Range] -> Int
    total (s, e) [] = e - s + 1
    total (s, e) ((is, ie) : rest)
      | is > e = e - s + 1 + total (is, ie) rest
      | otherwise = total (s, max e ie) rest

rowRangeTotal :: Int -> [(Point, Int)] -> Int
rowRangeTotal row ssAndRs = rangeTotal (rowSensorRanges row ssAndRs)

beacons :: [(Point, Point)] -> Set.Set Point
beacons = Set.fromList . map snd

-- If we were repeatedly finding the number of beacons in rows, it would be
-- more efficient to build up a Map of rows to beacon numbers. But we're not.
nBeaconsInRow :: Int -> [(Point, Point)] -> Int
nBeaconsInRow row ssAndBs = length $ filter (== row) beaconYs
  where
    beaconYs = map snd $ Set.toList (beacons ssAndBs)

nRowPointsNoBeacon :: Int -> [(Point, Point)] -> Int
nRowPointsNoBeacon row ssAndBs = rowRangeTotal row ssAndRs - nBeaconsInRow row ssAndBs
  where
    ssAndRs = sensorsAndRadii ssAndBs

resultFromLines :: [String] -> Int
resultFromLines = nRowPointsNoBeacon 2000000 . map parseLine

run :: [String] -> String
run ss = "Number of positions that cannot contain a beacon: " ++ show (resultFromLines ss)
