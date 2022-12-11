module Advent2022.Day09.Base (moveTail, applyMoveAndUpdate, applyLines, numTailVisited) where

import Data.Bifunctor (bimap, first, second)
import qualified Data.Set as Set

type Point = (Int, Int)

type PointSet = Set.Set Point

{-
  For Part 1, a rope was represented as a `(head, tail)` tuple.
  For Part 2, I just changed it to a list of points, one per knot, with the
  tail being the last one, so it can work with a rope of any length. The only
  change needed was using `scanl` in `applyMove` to move all the knots, and
  passing a number to `applyMoves` to create an initial rope with that many
  knots.

  Time complexity `O(n*k)` for `n` moves with a rope with `k` knots.
  (a "move" meaning one individual movement: "R 5" is 5 moves)
-}

type Rope = [Point]

type RopeAndSet = ([Point], PointSet)

moveTail :: Point -> Point -> Point
moveTail (hx, hy) t@(tx, ty)
  | isTouching = t
  | otherwise = (tx + signum dx, ty + signum dy)
  where
    dx = hx - tx
    dy = hy - ty
    isTouching = abs dx <= 1 && abs dy <= 1

applyMove :: (Point -> Point) -> Rope -> Rope
applyMove f ps = scanl1 moveTail psWithNewHead
  where
    psWithNewHead = f (head ps) : tail ps

applyMoveAndUpdate :: (Point -> Point) -> RopeAndSet -> RopeAndSet
applyMoveAndUpdate f (ps, pset) = (ps', Set.insert (last ps') pset)
  where
    ps' = applyMove f ps

applyMoves :: Int -> [[Point -> Point]] -> RopeAndSet
applyMoves ropeLength fss = moveFunc (replicate ropeLength origin, Set.singleton origin)
  where
    moveFuncs = map applyMoveAndUpdate (concat fss)
    moveFunc = foldr1 (flip (.)) moveFuncs
    origin = (0, 0)

parseMoveChar :: Char -> (Point -> Point)
parseMoveChar 'R' = first (+ 1)
parseMoveChar 'U' = second (+ 1)
parseMoveChar 'L' = first (subtract 1)
parseMoveChar 'D' = second (subtract 1)
parseMoveChar c = error $ "Invalid move character: " ++ show c

parseMove :: (Char, Int) -> [Point -> Point]
parseMove (c, n) = replicate n (parseMoveChar c)

parseLine :: String -> (Char, Int)
parseLine = bimap head read . splitAt 1

applyLines :: Int -> [String] -> RopeAndSet
applyLines ropeLength = applyMoves ropeLength . map (parseMove . parseLine)

numTailVisited :: Int -> [String] -> Int
numTailVisited ropeLength = Set.size . snd . applyLines ropeLength
