module Advent2022.Day15.Base
  ( parseLine,
    sensorsAndRadii,
    xBounds,
    beacons,
    inRadius,
  )
where

import qualified Data.Set as Set
import Lib.Parsing (intParser, parseOrError)
import Lib.Types (Point)
import Text.Parsec
import Text.Parsec.String

pointParser :: Parser Point
pointParser = (,) <$> (string "x=" *> intParser <* string ", y=") <*> intParser

stringThenPointParser :: String -> Parser Point
stringThenPointParser s = string s *> pointParser

lineParser :: Parser (Point, Point)
lineParser = (,) <$> stringThenPointParser "Sensor at " <*> stringThenPointParser ": closest beacon is at "

parseLine :: String -> (Point, Point)
parseLine = parseOrError lineParser

{-
  Determine if a position is covered by a beacon: Find the distance from a
  sensor to its closest beacon, and use this as a radius: Within this radius
  there cannot be any other beacons.

  So if a point is within any sensor's radius, and doesn't contain a beacon,
  then it cannot contain a beacon.

  To find a sensor's radius: just find the Manhattan distance between it and
  its closest beacon. Manhattan distance is clearly the sum of the absolute
  differences between the x and y coordinates.

  Then the obvious solution is to just check if each point is in any sensor's
  radius, by comparing the distance from the point to the sensor with the
  sensor's radius. The radii can be pre-calculated. There's likely to be a more
  efficient method when doing repeated calculations, though.
-}

manDist :: Point -> Point -> Int
manDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

sensorsAndRadii :: [(Point, Point)] -> [(Point, Int)]
sensorsAndRadii ssAndBs = [(s, manDist s b) | (s, b) <- ssAndBs]

beacons :: [(Point, Point)] -> Set.Set Point
beacons = Set.fromList . map snd

inRadius :: Point -> (Point, Int) -> Bool
inRadius p1 (p2, r) = manDist p1 p2 <= r

{-
  The text is quite misleading about the bounds of the area: the example gives
  the impression that it's the minimum area containing all beacons and sensors.
  But I eventually figured out that it needs to contain the radii for all
  sensors, which could expand beyond the outermost beacon locations, in order
  to find find all beacon-free cells in a row. So we need the lowest leftmost
  and highest rightmost edge positions of all sensor's radii.
-}
xBounds :: [(Point, Int)] -> (Int, Int)
xBounds ssAndRs = (minX, maxX)
  where
    minX = minimum $ map (\((x, _), r) -> x - r) ssAndRs
    maxX = maximum $ map (\((x, _), r) -> x + r) ssAndRs
