module Advent2022.Day12.Part2
  ( startPoints,
    bfsAllLines,
    run,
  )
where

import Advent2022.Day12.Base
import qualified Data.Dequeue as DQ
import qualified Data.Matrix as M
import Data.Maybe (catMaybes)

-- Find every point height 0 (from char 'a').
-- Again it would be a little faster to do this during the parse, but it's a
-- one-off job and to get part 2 done quickly with the part 1 code this is nice
-- and easy.
startPoints :: Hills -> [Point]
startPoints = map fst . filter ((== 0) . snd) . M.toList . M.mapPos (,)

bfsAll :: [Point] -> Point -> Hills -> Int
bfsAll starts end hs = minimum (catMaybes paths)
  where
    paths = map (\s -> bfs end (DQ.fromList [s]) hs (initialDistances hs)) starts

bfsAllLines :: [String] -> Int
bfsAllLines ss = bfsAll (startPoints hs) end hs
  where
    (hs, _, end) = parseLines ss

run :: [String] -> String
run ss = "Result: " ++ show (bfsAllLines ss)
