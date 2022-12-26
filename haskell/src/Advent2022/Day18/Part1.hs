module Advent2022.Day18.Part1
  ( run,
    connected,
    nConnected,
    surfaceArea,
  )
where

import Advent2022.Day18.Base
import Data.List (tails)

{-
  A cube has six faces, so surface area 6. If two cubes are connected, that
  means 1 face of each is connected, so 2 faces. So if `n` cubes are connected,
  the surface area is `6n - 2(n-1)`, and for any `n` cubes with `k` connections,
  it is `6n - 2k`.

  So we just need to find how many cubes are connected, which is the number of
  points that are adjacent: 1 apart on one axis and on equal on the others. So
  find all combinations of points and check which ones are adjacent.
-}

connected :: Point -> Point -> Bool
connected (x1, y1, z1) (x2, y2, z2) =
  adj x1 x2 && y1 == y2 && z1 == z2
    || x1 == x2 && adj y1 y2 && z1 == z2
    || x1 == x2 && y1 == y2 && adj z1 z2
  where
    adj a b = abs (a - b) == 1

nConnected :: [Point] -> Int
nConnected ps = length $ filter (uncurry connected) pairs
  where
    pairs = [(a, b) | (a : bs) <- tails ps, b <- bs]

surfaceArea :: [Point] -> Int
surfaceArea ps = length ps * 6 - nConnected ps * 2

surfaceAreaFromLines :: [String] -> Int
surfaceAreaFromLines = surfaceArea . parseLines

run :: [String] -> String
run ss = "Result: " ++ show (surfaceAreaFromLines ss)
