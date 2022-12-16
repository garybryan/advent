module Advent2022.Day14.Base
  ( Point,
    Points,
    parseLine,
    pointsOnLine,
    pointsOnLines,
    pointsOnAllLines,
    maxYOfPoints,
  )
where

import qualified Data.Set as Set
import Lib.Parsing (parseOrError, pointParser)
import Lib.Types (Point)
import Text.Parsec
import Text.Parsec.String

type Points = Set.Set Point

lineParser :: Parser [Point]
lineParser = pointParser `sepBy` string " -> "

parseLine :: String -> [Point]
parseLine = parseOrError lineParser

{-
  Use a Set of Points to track which tiles are full; it doesn't matter whether
  with rock or sand, the behaviour is the same. Scan the given lines to get
  the points already filled with rock, and then add sand as we go.

  A Matrix to represent the grid would be another option, but that would require
  knowing the size of the grid in advance and would likely take up more space
  than is really needed.

  But to detect sand falling into the void, we need to know the highest y value
  of any filled tile, so once filling in the Set, this needs to be found.
  (Tracking a max while building the set would avoid a loop but make the code
  much less clean.)
-}

-- Get all the points on a single horizontal or vertical line defined by start
-- and end points.
pointsOnLine :: Point -> Point -> [Point]
pointsOnLine s@(sx, sy) e@(ex, ey)
  | sy > ey || sx > ex = pointsOnLine e s
  | sx == ex = [(sx, y) | y <- [sy .. ey]]
  | otherwise = [(x, sy) | x <- [sx .. ex]]

pointsOnLines :: [Point] -> Points
pointsOnLines ps = Set.fromList $ concatMap (uncurry pointsOnLine) $ zip ps (tail ps)

pointsOnAllLines :: [[Point]] -> Points
pointsOnAllLines = foldr1 Set.union . map pointsOnLines

maxYOfPoints :: Points -> Int
maxYOfPoints = Set.findMax . Set.map snd
