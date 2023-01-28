module Advent2022.Day16.Part2
  ( maxPressures,
    pairs,
    pairsMax,
    run,
  )
where

import Advent2022.Day16.Base
import Advent2022.Day16.Parsing
import Data.Bits
import qualified Data.Map as Map

type ValvesPair = (BitVector, BitVector)

{-
  The idea is to find the two paths with disjoint sets of opened valves that
  give the highest combined pressure, since the player and the elephant will
  always open different valves.

  With my previous DP solution, the only option was to run the whole search
  for all disjoint pairs of open valves (or openable valves; with bit vectors
  they're complementary). But an algorithm run an exponential number of
  times is never really good news: it took 15 minutes, and it didn't always
  give correct results after all that.

  The B&B solution can be modified slightly to store the max pressure for each
  combination of open valves as such pressures are encountered during the
  search, using the same lower-bound logic as before. Then the disjoint pairs
  can be found (still exponential, but it's not multiplying anything now!)
  along with their max totals.

  I also found that for some inputs, including the real puzzle data, the upper
  bound function for the B&B meant that some branches with optimal disjoint
  solutions were being pruned because they didn't make for an optimal overall
  (one-actor) solution. So I added a flag to enable/disable the upper bound, in
  order to keep the ~10x speedup it gives for part 1 but also ensure a correct
  solution for part 2.
-}

maxPressures :: Int -> Valves -> Int -> Maxes
maxPressures limit valves startValveIndex =
  snd $ maxPressureAndPressures limit valves False startValveIndex

{-
  Generate all unique pairs of items.

  Pairs are combinations of length 2.

  Combinations are like permutations but with no repetition as we don't want
  duplicate pairs: it doesn't make a difference which set of valves the player
  opens and which the elephant opens.

  Repetition is avoided by only combining each item with all subsequent items.
-}
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = [(x, y) | y <- xs] ++ pairs xs

disjointPairs :: [ValvesPair] -> [ValvesPair]
disjointPairs = filter isDisjoint
  where
    isDisjoint (v1, v2) = v1 .&. v2 == 0

pairTotals :: Maxes -> [Int]
pairTotals maxes = map maxSum $ disjointPairs $ pairs (Map.keys maxes)
  where
    maxSum (v1, v2) = maxes Map.! v1 + maxes Map.! v2

pairsMax :: Int -> Valves -> Int -> Int
pairsMax limit valves start = maximum $ pairTotals $ maxPressures limit valves start

pairsMaxFromLines :: [String] -> Int
pairsMaxFromLines = uncurry (pairsMax 26) . valveVectorAndStartIndexFromLines

run :: [String] -> String
run ss = "Max pressure from player and elephant opening valves: " ++ show (pairsMaxFromLines ss)
