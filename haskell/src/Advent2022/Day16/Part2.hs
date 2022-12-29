module Advent2022.Day16.Part2
  ( partitions,
    pairTotal,
    pairsMax,
    run,
  )
where

import Advent2022.Day16.Base
import Data.Bits
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map

{-
  I spent far too long trying to somehow adapt the Part 1 DP solution to work
  for part 2, either by running two searches at once with disjoint valves or by
  keeping track of states, but I was just getting nowhere so did a bit of a
  bodge instead...

  The idea is to find the two paths with disjoint sets of opened valves that
  give the highest combined pressure, since the player and the elephant will
  always open different valves.

  So part 1 is amended to take a set of valves that can be opened, and is then
  run for each pair of disjoint subsets of valves.

  Finding possible disjoint subsets is just a case of finding all partitions
  of the set into two sets. I've done this with some binary trickery:
  Generate all numbers from 1 to 2^(n - 1) - 1, which gives all combinations of
  bit 1 to n - 1 being 0 or 1; we don't care about bit zero or n because we
  want each actor to open at least one valve. The list is then partitioned
  based on the i-th bit being set for each of these numbers.

  Time complexity O(2^V * V^2 * M) for V valves in M minutes, which is frankly
  embarrassing. Took 16 minutes on my computer (and the answer was still
  wrong...). Space complexity should be O(2^V + V^2 * M) since only one table
  needs to be held in memory at once and it only needs to compare the maxes.
-}

-- TODO just use bit vectors directly instead of messing about with sets?
partitions :: ValveSet -> [(ValveSet, ValveSet)]
partitions nums = map part ([1 .. 1 `shiftL` (len - 1) - 1] :: [Int])
  where
    len = IntSet.size nums
    indices = Map.fromList $ zip (IntSet.toList nums) [0 ..]
    part i = IntSet.partition (testBit i . (indices Map.!)) nums

pairTotal :: Int -> ValveMap -> Int -> (ValveSet, ValveSet) -> Int
pairTotal limit vm start (v1, v2) = mp v1 + mp v2
  where
    mp vs = maxPressure limit vm vs start

pairsMax :: Int -> ValveMap -> Int -> Int
pairsMax limit vm start = maximum pairTotals
  where
    valves = nonZeroValves vm
    pairTotals = map (pairTotal limit vm start) (partitions valves)

pairsMaxFromLines :: [String] -> Int
pairsMaxFromLines = uncurry (pairsMax 26) . valveMapAndStartIndexFromLines

run :: [String] -> String
run ss = "Max pressure from player and elephant opening valves: " ++ show (pairsMaxFromLines ss)
