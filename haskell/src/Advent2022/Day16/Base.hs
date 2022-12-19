module Advent2022.Day16.Base
  ( Valve (..),
    ValveMap,
    parseLine,
    valveMapAndStartIndexFromLines,
    maxPressure,
    makeTable,
  )
where

import qualified Data.IntSet as IntSet
import Data.List (maximumBy)
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Lib.Parsing (intParser, parseOrError)
import Text.Parsec
import Text.Parsec.String

{-
  This looks like a cross between a graph search and a knapsack-style
  optimisation problem: maximise the pressure given the time costs to do so.

  Could approximate with a greedy algorithm of always going for the valve with
  the highest pressure, but that won't always be optimal, as shown in the
  example.

  I tried a backtracking DFS solution first but it was just far too slow...
  Bottom-up dynamic programming to the rescue.

  The graph is represented with adjacency lists. A dynamic programming table
  with one row per minute and one column per valve is used to store the optimal
  result so far for ending on a given valve at a given minute, and is filled in
  row-by-row. Each cell stores the total pressure, the total flow rate for all
  open valves, and the indices of the open valves.

  A bit like the knapsack problem, at each cell there is a choice of either
  opening the valve or not opening it. Unlike knapsack, a valve can only be
  opened if enough time has elapsed to reach it, which is simulated by storing
  an Unreachable placeholder in cells until they can be reached from an
  adjacent valve in the previous minute.

  To avoid unnecessary work, valves with flow rate zero are not considered for
  opening.

  For opening a valve, all the possibilties for reaching it from adjacent
  nodes are considered so that the optimal path to take before opening it can
  be selected. Since moving to and then opening a valve takes 2 minutes, the
  rates from adjacent valves on paths 2 minutes ago on which this valve has not
  already been opened (and their pressures plus 2 times the rate at that time,
  to get the current pressure at the time of opening this valve) are
  considered.

  For not opening a valve, choose the most optimal adjacent valve to arrive
  from, considering the pressure a minute ago plus the rate to get the pressure
  at this time. Not moving from the current valve would also be an option, but
  never gives an optimal result so isn't considered.

  The base case is the first minute, where the rate and pressure at the starting
  valve are zero, and all others are unreachable.

  Of all the choices, the optimal one is chosen by calculating what the
  pressure would be at the end of the time limit if it were to be opened, by
  adding the potential current pressure to the potential current rate
  multiplied by the number of remaining minutes. Choosing just by pressure
  isn't enough, because a high rate means that pressure will grow more quickly
  over time.
-}

type ValveName = String

type ValveIndex = Int

type AdjList = [ValveIndex]

type NamedValve = (ValveName, (Int, [ValveName]))

data Valve = Valve Int AdjList deriving (Eq, Show)

type ValveMap = Map.Map ValveIndex Valve

valveNameParser :: Parser ValveName
valveNameParser = many letter

sourceValveParser :: Parser ValveName
sourceValveParser = string "Valve " *> valveNameParser

flowParser :: Parser Int
flowParser = string " has flow rate=" *> intParser

destValvesParser :: Parser [ValveName]
destValvesParser = valveNameParser `sepBy` string ", "

valveParser :: Parser (Int, [String])
valveParser =
  (,)
    <$> flowParser
    <*> ( string "; tunnel"
            *> (string "s lead to valves " <|> string " leads to valve ")
            *> destValvesParser
        )

lineParser :: Parser NamedValve
lineParser = (,) <$> sourceValveParser <*> valveParser

parseLine :: String -> NamedValve
parseLine = parseOrError lineParser

-- Store valves with indices rather than names, so they can be used in a table
-- (and for a minimal performance gain).
-- Valve "AA" isn't necessarily the first in the input, so we need to be able
-- to find its index in order to know where to start.
valveMapAndStartIndex :: [NamedValve] -> (ValveMap, Int)
valveMapAndStartIndex namesAndVals = (vm, index "AA")
  where
    nameIndices = Map.fromList $ zip (map fst namesAndVals) [1 ..]
    index name = fromMaybe (error $ "Unknown valve: " ++ name) $ Map.lookup name nameIndices
    entry (name, (rate, adjNames)) = (index name, Valve rate (map index adjNames))
    vm = Map.fromList $ map entry namesAndVals

valveMapAndStartIndexFromLines :: [String] -> (ValveMap, Int)
valveMapAndStartIndexFromLines = valveMapAndStartIndex . map parseLine

data TableCell
  = Unreachable
  | Cell Int Int IntSet.IntSet

instance Show TableCell where
  -- More compact output than the derived Show, handy for debugging.
  show (Cell p r s) = show (p, r, IntSet.toList s)
  show _ = "Unreachable"

type DPTable = M.Matrix TableCell

emptyCell :: TableCell
emptyCell = Cell 0 0 IntSet.empty

isReachable :: TableCell -> Bool
isReachable Unreachable = False
isReachable _ = True

potentialPressure :: Int -> TableCell -> Int
potentialPressure remainingMins (Cell p r _) = p + r * remainingMins
potentialPressure _ _ = 0

-- This could be optimised to use O(V) space rather than O(V*M), since values
-- are only derived from the previous two rows, but that's an exercise for later.
-- First I should tidy it up and refactor some logic out of the massive "where" clause.
dp :: Int -> Int -> ValveMap -> DPTable -> Int -> Int -> DPTable
dp limit maxValveI vm table minute valveI
  | minute > limit && valveI > maxValveI = table -- Finished bottom row.
  | valveI > maxValveI = recurse table (minute + 1) 1 -- Finished a row; move onto next.
  | otherwise = recurse table' minute (valveI + 1)
  where
    recurse = dp limit maxValveI vm
    Valve curValveRate adj = fromJust $ Map.lookup valveI vm
    maxByPotentialPressure = maximumBy $ comparing (potentialPressure (limit - minute))
    prevWithCurPressure vi = case table M.! (minute - 1, vi) of
      Cell p r s -> Just $ Cell (p + r) r s
      _ -> Nothing
    noOpenChoices
      | minute == 1 && isReachable (table M.! (minute, valveI)) = [emptyCell]
      | minute == 1 = []
      | otherwise = mapMaybe prevWithCurPressure adj
    bestNoOpenChoice
      | null noOpenChoices = Nothing
      | otherwise = Just $ maxByPotentialPressure noOpenChoices
    prevWithCurPressureOpening vi = case table M.! (minute - 2, vi) of
      Cell p r ovs -> Just $ Cell (p + r * 2) (r + curValveRate) ovs
      _ -> Nothing
    alreadyOpen (Cell _ _ ovs) = valveI `IntSet.member` ovs
    alreadyOpen _ = False
    addValveToSet (Cell p r ovs) = Cell p r $ IntSet.insert valveI ovs
    addValveToSet c = c
    adjOpenChoices
      | minute <= 2 = []
      | otherwise = filter (not . alreadyOpen) $ mapMaybe prevWithCurPressureOpening adj
    openChoices = Cell 0 curValveRate IntSet.empty : adjOpenChoices
    bestOpenChoice
      | null adjOpenChoices = Nothing
      | otherwise = Just $ addValveToSet (maxByPotentialPressure openChoices)
    bestChoices = catMaybes [bestNoOpenChoice, bestOpenChoice]
    bestChoice = maxByPotentialPressure bestChoices
    table'
      | null bestChoices = table
      | otherwise = M.setElem bestChoice (minute, valveI) table

makeTable :: Int -> ValveMap -> Int -> DPTable
makeTable limit vm startIndex = dp limit nValves vm table 1 1
  where
    table = M.matrix (limit + 1) nValves firstReachableOnly
    firstReachableOnly pos
      | pos == (1, startIndex) = emptyCell
      | otherwise = Unreachable
    nValves = Map.size vm

maxPressure :: Int -> ValveMap -> Int -> Int
maxPressure limit vm startIndex = maximum $ mapMaybe pressure (V.toList finalRow)
  where
    finalTable = makeTable limit vm startIndex
    finalRow = M.getRow (limit + 1) finalTable
    pressure (Cell p _ _) = Just p
    pressure _ = Nothing
