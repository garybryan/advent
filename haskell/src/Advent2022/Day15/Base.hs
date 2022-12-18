module Advent2022.Day15.Base
  ( Range,
    parseLine,
    sensorsAndRadii,
    rowSensorRange,
    rowSensorRanges,
  )
where

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Lib.Parsing (intParser, parseOrError)
import Lib.Types (Point)
import Text.Parsec
import Text.Parsec.String

-- Same thing as a Point, but different alias for clarity.
type Range = (Int, Int)

pointParser :: Parser Point
pointParser = (,) <$> (string "x=" *> intParser <* string ", y=") <*> intParser

stringThenPointParser :: String -> Parser Point
stringThenPointParser s = string s *> pointParser

lineParser :: Parser (Point, Point)
lineParser = (,) <$> stringThenPointParser "Sensor at " <*> stringThenPointParser ": closest beacon is at "

parseLine :: String -> (Point, Point)
parseLine = parseOrError lineParser

manDist :: Point -> Point -> Int
manDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

sensorsAndRadii :: [(Point, Point)] -> [(Point, Int)]
sensorsAndRadii ssAndBs = [(s, manDist s b) | (s, b) <- ssAndBs]

-- Based on Manhattan distance, the x-range of a sensor for a row is the
-- sensor's x position plus and minus the radius minus the absolute difference
-- between the row and the sensor's y position.
-- If that difference is negative, that sensor doesn't cover the row.
rowSensorRange :: Int -> (Point, Int) -> Maybe Range
rowSensorRange row ((x, y), r)
  | rowRadius < 0 = Nothing
  | otherwise = Just (x - rowRadius, x + rowRadius)
  where
    rowRadius = r - abs (row - y)

rowSensorRanges :: Int -> [(Point, Int)] -> [Range]
rowSensorRanges row = mapMaybe (rowSensorRange row)
