module Advent2022.Day12.Base
  ( Point,
    Hills,
    parseLines,
    weight1Neighbours,
    initialDistances,
    bfs,
    bfsFrom,
  )
where

import Data.Char (ord)
import qualified Data.Dequeue as DQ
import Data.List (elemIndex)
import qualified Data.Matrix as M
import Data.Maybe
import Lib.Matrix (matrixFoldr)

{-
  Looks like a fairly classic shortest-path graph search, with the limitation
  that you can only go to the next node if the difference between heights is at
  most 1. This limitation turns it into an unweighted graph, since all edges
  have equal weight (1 step), so we can just use a bog-standard Breadth First
  Search where we track the distance so far and as soon as we hit the end node
  we've found the shortest path. No need to bust out the Dijkstra... yet.

  Use a matrix to keep track of the distances for each position on the grid,
  with zero for unexplored nodes. A Set would be another good option.

  In Haskell, Chars are in the Ord class but not Num, so we can compare them
  but not subtract them to get height differences. So might as well just parse
  them all into Ints and deal with these.

  Complexity O(n) where n is number of hills. The complexity of a graph search
  is O(V + E) for V vertices and E edges, since at worst it has to process each
  once, and here the edges scale linearly with the vertices as there are 4
  potential edges per vertex.
-}

type Hills = M.Matrix Int

type Distances = M.Matrix Int

type Point = (Int, Int)

type PointQ = DQ.BankersDequeue Point

parseChar :: Char -> Int
parseChar 'S' = parseChar 'a'
parseChar 'E' = parseChar 'z'
parseChar c = ord c - ord 'a'

parseLines :: [String] -> (Hills, Point, Point)
parseLines ls = (M.fromLists $ map (map parseChar) ls, findPoint 'S' ls, findPoint 'E' ls)

-- Quick and dirty method to find the coordinates of the start and end points.
-- Of course it would be faster to do the parsing and the point finding in the
-- same loop, but this is a bit more simple and readable and it only happens
-- once.
findPoint :: Char -> [String] -> Point
findPoint c ss = go $ zip [1 ..] ss
  where
    go :: [(Int, String)] -> Point
    go [] = error $ "Character " ++ show c
    go ((row, s) : rsAndSs) = case elemIndex c s of
      Nothing -> go rsAndSs
      Just col0 -> (row, col0 + 1)

neighbours :: Point -> [Point]
neighbours (row, col) = [(row - 1, col), (row + 1, col), (row, col + 1), (row, col - 1)]

validNeighbours :: Hills -> [Point] -> [Point]
validNeighbours hs = filter validPoint
  where
    validPoint (row, col) = row >= 1 && col >= 1 && row <= M.nrows hs && col <= M.ncols hs

-- Weight is not commutative, since we can only step up 1 but can step down any.
weight :: Point -> Point -> Hills -> Int
weight p1 p2 hs = hs M.! p2 - hs M.! p1

weight1Neighbours :: Point -> Hills -> [Point]
weight1Neighbours p hs = filter weight1 $ validNeighbours hs (neighbours p)
  where
    weight1 :: Point -> Bool
    weight1 nP = weight p nP hs <= 1

initialDistances :: Hills -> Distances
initialDistances hs = M.zero (M.nrows hs) (M.ncols hs)

-- TL;DR:
-- Pop a point off the queue. If it's the target then return the distance to it.
-- Otherwise find all the unseen weight-1 neighbours, put them onto the queue,
-- update their distances to the distance to the current point plus one, and
-- keep on searchin'.
bfs :: Point -> PointQ -> Hills -> Distances -> Maybe Int
bfs target q hs ds
  | q == DQ.empty = Nothing
  | p == target = Just dP
  | otherwise = bfs target q'' hs ds'
  where
    (p, q') = fromJust $ DQ.popFront q -- We've checked the queue's not empty, so we can live dangerously.
    ns = filter ((== 0) . (M.!) ds) (weight1Neighbours p hs)
    q'' = foldl DQ.pushBack q' ns
    dP = ds M.! p
    ds' = matrixFoldr (M.setElem (dP + 1)) ds ns

bfsFrom :: Point -> Point -> Hills -> Int
bfsFrom start end hs = fromMaybe (error "No path found") $ bfs end (DQ.fromList [start]) hs (initialDistances hs)
