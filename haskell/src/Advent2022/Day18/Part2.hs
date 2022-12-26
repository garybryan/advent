module Advent2022.Day18.Part2
  ( boundingBox,
    externalSurfaceArea,
    run,
  )
where

import Advent2022.Day18.Base
{-
  Consider external surface area only. Either find the whole surface area from
  part 1 and then calculate and subtract the internal surfaces, or do a
  calculation that only counts external surfaces in the first place.

  I couldn't think of an easy way to count internal surfaces, so just did a
  flood-fill style breadth-first search to count every external face.

  Find the bounding box for the whole shape leaving 1 unit clear from every
  edge. Then start a 3-dimensional BFS from the minimum corner (although any
  corner would do) that counts the number of adjacent cube points for each
  point and adds them to a running total.
-}

import qualified Data.Dequeue as DQ
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type PointQueue = DQ.BankersDequeue Point

-- Not the most efficient way to get all the mins and maxes, but it works.
boundingBox :: [Point] -> (Point, Point)
boundingBox ps =
  ( (minimum xs - 1, minimum ys - 1, minimum zs - 1),
    (maximum xs + 1, maximum ys + 1, maximum zs + 1)
  )
  where
    (xs, ys, zs) = unzip3 ps

nextPoints :: Point -> [Point]
nextPoints (x, y, z) =
  [ (x - 1, y, z),
    (x + 1, y, z),
    (x, y - 1, z),
    (x, y + 1, z),
    (x, y, z - 1),
    (x, y, z + 1)
  ]

externalSurfaceArea :: [Point] -> Int
externalSurfaceArea points = bfs Set.empty (DQ.fromList [start]) 0
  where
    (start@(minX, minY, minZ), (maxX, maxY, maxZ)) = boundingBox points
    inBounds (x, y, z) = x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ
    pointSet = Set.fromList points
    bfs :: Set.Set Point -> PointQueue -> Int -> Int
    bfs seen q nFaces
      | DQ.null q = nFaces
      | otherwise = bfs seen' q' nFaces'
      where
        (point, pointlessQ) = fromJust $ DQ.popFront q
        unseenNeighbours = filter (\p -> inBounds p && p `Set.notMember` seen) $ nextPoints point
        (cubes, notCubes) = partition (`Set.member` pointSet) unseenNeighbours
        seen' = foldr Set.insert seen (point : notCubes)
        q' = foldl DQ.pushBack pointlessQ notCubes
        nFaces' = nFaces + length cubes

externalSurfaceAreaFromLines :: [String] -> Int
externalSurfaceAreaFromLines = externalSurfaceArea . parseLines

run :: [String] -> String
run ss = "Result: " ++ show (externalSurfaceAreaFromLines ss)
