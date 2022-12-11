module Advent2022.Day08.Part2 (scenicScoreDir, scenicScore, scenicScores, maxScenicScore, run) where

import Advent2022.Day08.Base
import Data.Bifunctor (first, second)
import qualified Data.Matrix as M

{-
  Basic approach: calculate as described in the challenge, by going out from
  the point and counting how many trees can be seen in each direction then
  multiplying the results together.

  This is O(num rows + num cols), so finding the score for every tree is
  O((rows * cols) * (rows + cols)), so O(n^3) for an n*n matrix. Not great.

  On any edge, no trees can be seen in at least one direction, so the score
  will always be zero. Check for that case to avoid doing unnecessary work.

  Otherwise, a tree blocks the view if it is as tall or taller. Count it, but
  stop looking for successive trees.

  TODO: can this be optimised with prefix-sum-style calculations?
-}

-- Predefined functions for moving one right, one left, one up, and one down in a matrix.
dirFunctions :: [(Int, Int) -> (Int, Int)]
dirFunctions = [first (+ 1), first (subtract 1), second (+ 1), second (subtract 1)]

scenicScore :: TreeMatrix -> (Int, Int) -> Int -> Int
scenicScore m (r, c) h = product $ map (scenicScoreDir m (r, c) h) dirFunctions

scenicScoreDir :: TreeMatrix -> (Int, Int) -> Int -> ((Int, Int) -> (Int, Int)) -> Int
scenicScoreDir m p@(r, c) h f
  | r == 1 || c == 1 || r == M.nrows m || c == M.ncols m = 0
  | m M.! p' >= h = 1
  | otherwise = 1 + scenicScoreDir m p' h f
  where
    p' = f p

-- A TreeMatrix is a Matrix Int; they're just written differently here for
-- clarity since the result matrix represents scores rather than trees.
scenicScores :: TreeMatrix -> M.Matrix Int
scenicScores m = M.mapPos (scenicScore m) m

maxScenicScore :: TreeMatrix -> Int
maxScenicScore = maximum . scenicScores

run :: [String] -> String
run ss = "Highest scenic score: " ++ show (maxScenicScore $ parseLines ss)
