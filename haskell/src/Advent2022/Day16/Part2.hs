module Advent2022.Day16.Part2
  (
  )
where

-- run,
-- makeTable,

import Advent2022.Day16.Base
import Data.Bifunctor (bimap)
import qualified Data.IntSet as IntSet
import Data.List (maximumBy)
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Vector as V

{-
  Get all unique pairs of valves.
  Then get all pairs of states where the patterns match.
  Then add their potential pressures and find the max.

-}

-- Pairs of open valves and potential pressures.
-- They're going into a tree set, so put the Int first for quicker sorting.
-- Sorting by potential pressure might come in useful too.
type CellState = (Int, IntSet.IntSet)

data Cell = Cell Int Int IntSet.IntSet

instance Show Cell where
  -- More compact output than the derived Show, handy for debugging.
  show (Cell p r s) = show (p, r, IntSet.toList s)

type DPTable = M.Matrix [Cell]

type States = Set.Set CellState

-- cellState :: Int -> TableCell -> CellState
-- cellState remainingMins c@(Cell _ _ s) = (potentialPressure remainingMins c, s)
-- cellState _ _ = error "Can only get cell state from a reachable cell."

emptyCell :: Cell
emptyCell = Cell 0 0 IntSet.empty

amendIfReachable :: ((Int, Int) -> (Int, Int)) -> [Cell] -> [Cell]
amendIfReachable f = map cellF
  where
    cellF cell = let (Cell p r s) = cell; (p', r') = f (p, r) in Cell p' r' s

nonOpenChoices :: DPTable -> Int -> ValveIndex -> ValveMap -> [Cell]
nonOpenChoices table minute valveI vm
  | minute == 1 && not (null $ table M.! (minute, valveI)) = [emptyCell]
  | minute == 1 = []
  | otherwise = concatMap prevWithCurPressure adj
  where
    Valve _ adj = fromJust $ Map.lookup valveI vm
    prevWithCurPressure vi = amendIfReachable (\(p, r) -> (p + r, r)) $ table M.! (minute - 1, vi)

alreadyOpen :: ValveIndex -> Cell -> Bool
alreadyOpen valveI (Cell _ _ ovs) = valveI `IntSet.member` ovs

openValve :: ValveIndex -> Cell -> Cell
openValve valveI (Cell p r ovs) = Cell p r $ IntSet.insert valveI ovs

openChoices :: DPTable -> Int -> ValveIndex -> ValveMap -> [Cell]
openChoices table minute valveI vm
  | rate == 0 = []
  | otherwise = map (openValve valveI) choices
  where
    Valve rate adj = fromJust $ Map.lookup valveI vm
    prevWithCurPressure vi = amendIfReachable (\(p, r) -> (p + r * 2, r + rate)) $ table M.! (minute - 2, vi)
    openCurValve
      | minute == 1 = []
      | otherwise = amendIfReachable (\(p, r) -> (p + r, r + rate)) $ table M.! (minute - 1, valveI)
    adjChoices
      | minute <= 2 = []
      | otherwise = concatMap prevWithCurPressure adj
    choices = filter (not . alreadyOpen valveI) $ openCurValve ++ adjChoices

dp :: Int -> Int -> ValveMap -> DPTable -> Int -> Int -> DPTable
dp limit maxValveI vm table minute valveI
  | minute > limit + 1 = table
  | valveI > maxValveI = recurse table (minute + 1) 1
  | otherwise = recurse table' minute (valveI + 1)
  where
    recurse = dp limit maxValveI vm
    nocs = nonOpenChoices table minute valveI vm
    ocs = openChoices table minute valveI vm
    choices = nocs ++ ocs
    table'
      | null choices = table
      | otherwise = M.setElem choices (minute, valveI) table

-- states' = foldr (Set.insert . cellState remainingMins) states choices

-- This could probably be more efficient using union find, but I've not studied
-- union find yet.
disjointCells :: States -> Set.Set (CellState, CellState)
disjointCells states = usefulStatePairs
  where
    valveSets = Set.map snd states
    valveSetPairs = Set.cartesianProduct valveSets valveSets
    disjointValveSetPairs = Set.filter (uncurry IntSet.disjoint) valveSetPairs
    statePairs = Set.cartesianProduct states states
    usefulStatePairs = Set.filter (\(a, b) -> (snd a, snd b) `Set.member` disjointValveSetPairs) statePairs
    pairs = Set.map (bimap fst fst) usefulStatePairs

makeTable :: Int -> ValveMap -> Int -> DPTable
makeTable limit vm startIndex = dp limit nValves vm initialTable 1 1
  where
    firstReachableOnly pos
      | pos == (1, startIndex) = [emptyCell]
      | otherwise = []
    initialTable = M.matrix (limit + 1) nValves firstReachableOnly
    nValves = Map.size vm
