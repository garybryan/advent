module Advent2022.Day15.Part1
  ( cannotHaveBeacon,
    rowPointsNoBeacon,
    nRowPointsNoBeacon,
    run,
  )
where

import Advent2022.Day15.Base
import qualified Data.Set as Set
import Lib.Types (Point)

cannotHaveBeacon :: [(Point, Int)] -> Set.Set Point -> Point -> Bool
cannotHaveBeacon ssAndRs bs p = p `Set.notMember` bs && any (inRadius p) ssAndRs

rowPointsNoBeacon :: Int -> [(Point, Point)] -> [Point]
rowPointsNoBeacon row ssAndBs = filter (cannotHaveBeacon ssAndRs bs) rowPs
  where
    ssAndRs = sensorsAndRadii ssAndBs
    (xMin, xMax) = xBounds ssAndRs
    rowPs = [(x, row) | x <- [xMin .. xMax]]
    bs = beacons ssAndBs

nRowPointsNoBeacon :: Int -> [(Point, Point)] -> Int
nRowPointsNoBeacon row ssAndBs = length $ rowPointsNoBeacon row ssAndBs

resultFromLines :: [String] -> Int
resultFromLines = nRowPointsNoBeacon 2000000 . map parseLine

run :: [String] -> String
run ss = "Number of positions that cannot contain a beacon: " ++ show (resultFromLines ss)
