module Advent2022.Day08.Part1
  ( visVectorList,
    leftVisMatrix,
    topVisMatrix,
    rightVisMatrix,
    bottomVisMatrix,
    visMatrix,
    numVisible,
    run,
  )
where

import Advent2022.Day08.Base
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Lib.Matrix (matrixFoldr)

{-
  This solution goes for functional cleverness over efficiency. Time complexity
  is `O(n)` for `n` trees, which is optimal, but a typical procedural approach
  of maintaining a single visibility matrix is almost certainly faster than
  doing a whole bunch of matrix operations and composing four of them together
  as is done here.
-}

type VisMatrix = M.Matrix Bool

vis :: (Bool, Int) -> Int -> (Bool, Int)
vis (_, y) x = (x > y, max x y)

-- Data.Matrix doesn't make it easy to apply a map to every row or column,
-- rather than a single one, so it's easiest to just convert them to lists and
-- then rebuild the matrix with `fromLists`.
visVectorList :: V.Vector Int -> [Bool]
visVectorList = map fst . tail . scanl vis (True, -1) . V.toList

leftVisMatrix :: TreeMatrix -> VisMatrix
leftVisMatrix m = M.fromLists [visVectorList $ M.getRow r m | r <- [1 .. M.nrows m]]

topVisMatrix :: TreeMatrix -> VisMatrix
topVisMatrix = M.transpose . leftVisMatrix . M.transpose

-- This could also be done by multiplying by a transposition matrix,
-- but let's keep it simple.
reverseMatrix :: M.Matrix a -> M.Matrix a
reverseMatrix = M.fromLists . map reverse . M.toLists

rightVisMatrix :: TreeMatrix -> VisMatrix
rightVisMatrix = reverseMatrix . leftVisMatrix . reverseMatrix

bottomVisMatrix :: TreeMatrix -> VisMatrix
bottomVisMatrix = M.transpose . reverseMatrix . leftVisMatrix . reverseMatrix . M.transpose

visMatrix :: TreeMatrix -> VisMatrix
visMatrix m = matrixFoldr (M.elementwise (||)) initial ms
  where
    initial = M.matrix (M.nrows m) (M.ncols m) (const False)
    fs = [leftVisMatrix, topVisMatrix, rightVisMatrix, bottomVisMatrix]
    ms = sequence fs m

numVisible :: VisMatrix -> Int
numVisible = sum . fmap fromEnum

numVisibleInMatrix :: TreeMatrix -> Int
numVisibleInMatrix = numVisible . visMatrix

run :: [String] -> String
run ss = "Number of visible trees: " ++ show (numVisibleInMatrix $ parseLines ss)
