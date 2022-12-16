module Advent2022.Day15.Part2
  ( distressBeaconPos,
    tuningFreq,
    run,
  )
where

import Advent2022.Day15.Base
import qualified Data.Set as Set
import Lib.Types (Point)

{-
  The basic solution will be to find the one position in the range that cannot
  contain a beacon. Which is likely to be... not fast.
-}

-- "can have the distress beacon" isn't quite the inverse of cannotHaveBeacon:
-- If it contains another beacon then it can't contain the distress beacon
-- OR any other beacon.
canHaveDistressBeacon :: [(Point, Int)] -> Set.Set Point -> Point -> Bool
canHaveDistressBeacon ssAndRs bs p = p `Set.notMember` bs && not (any (inRadius p) ssAndRs)

rowPointsDistressBeacon :: (Int, Int) -> Int -> [(Point, Point)] -> [Point]
rowPointsDistressBeacon (xMin, xMax) row ssAndBs = filter (canHaveDistressBeacon ssAndRs bs) rowPs
  where
    ssAndRs = sensorsAndRadii ssAndBs
    rowPs = [(x, row) | x <- [xMin .. xMax]]
    bs = beacons ssAndBs

distressBeaconPos :: (Int, Int) -> [(Point, Point)] -> Point
distressBeaconPos (xyMin, xyMax) ssAndBs = head $ concatMap rowPoints [xyMin .. xyMax]
  where
    rowPoints row = rowPointsDistressBeacon (xyMin, xyMax) row ssAndBs

tuningFreq :: Int -> Point -> Int
tuningFreq mult (x, y) = x * mult + y

distressBeaconTuningFreq :: (Int, Int) -> Int -> [(Point, Point)] -> Int
distressBeaconTuningFreq bounds mult = tuningFreq mult . distressBeaconPos bounds

resultFromLines :: [String] -> Int
resultFromLines = distressBeaconTuningFreq (0, 4000000) 4000000 . map parseLine

run :: [String] -> String
run ss = "Distress beacon tuning frequency: " ++ show (resultFromLines ss)
