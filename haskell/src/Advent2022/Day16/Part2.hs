module Advent2022.Day16.Part2
  ( doThing,
    -- run,
    makeTable,
  )
where

import Advent2022.Day16.Base hiding (makeTable)
import qualified Data.IntSet as IntSet
import Data.List (maximumBy)
import qualified Data.Map as Map
import qualified Data.Matrix as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Vector as V

{-
  Dynamic programming solution again, adapted for 2 actors on the map.

  There needs to be a row for each actor for each minute. This could be
  represented either as a table with double the rows, or as two tables. These
  options are equivalent, but with pros and cons for mental reasoning. I've
  gone for a single table: it breaks the "one row per minute" idea, but it
  avoids needing different logic for each actor to find the other's latest
  move.

  Choices depend on the previous actor's optimal move (to get the latest
  pressure and adjacency) and the previous minute (for adjacency; since both
  actors start at the same place, either can be used, but for simplicity use
  the current one: go back 2 rows for the previous minute, 4 for the one before
  that.

  This does mean that the previous player's optimal move (an O(V) calculation)
  needs to be found for each turn, but that doesn't increase the overall
  complexity as it only needs to be done once per row.

  - Non-opening choices (move from adjacent valve):
    - Pressure, rate, valves come from the other actor's previous best move
    - Adjacency check comes from last minute

  - Opening choices (moved from adjacent valve a minute ago and open):
    - Open current: pressure, rate, and valves (to check if can open) from
      other actor's best previous move.
    - Open from adjacent: pressure and rate from other actor latest (to account
      for their move); adjacency check from 2 minutes ago.

    Pressure only increases on the first player's turn!

    Is taking the previous player's best move always optimal? Or, like in part 1,
    is that an incorrect greedy strategy?
-}

doThing :: String
doThing = "Did a thing, part 2."

minuteForRow :: Int -> Int
minuteForRow row = (row + 1) `div` 2

-- 1 for the player's turn (odd), 0 for the elephant's turn (even)
-- Used as a multiplier so pressure only increases on the player's turn.
-- TODO will this allow for reuse: make generic for number of actors?
turnForRow :: Int -> Int
turnForRow row = row `mod` 2

nonOpenChoices :: DPTable -> Int -> ValveIndex -> ValveMap -> [TableCell]
nonOpenChoices table row valveI vm
  | minute == 1 && isReachable (table M.! (row, valveI)) = [emptyCell]
  | minute == 1 = []
  | otherwise = mapMaybe prevWithCurPressure (valveI : adj)
  where
    Valve _ adj = fromJust $ Map.lookup valveI vm
    minute = minuteForRow row
    turn = turnForRow row
    prevWithCurPressure vi
      -- \| minute > 1 && isReachable (table M.! (row - 2, vi)) =
      --     amendIfReachable (\(p, r) -> (p + r * turn, r)) $ table M.! (minute - 1, vi)
      | minute > 1 = amendIfReachable (\(p, r) -> (p + r, r)) $ table M.! (row - 2, vi)
      | otherwise = Nothing

openInPrevRow :: DPTable -> Int -> ValveIndex -> Bool
openInPrevRow table row valveI = alreadyOpen valveI (table M.! (row - 1, valveI))

openChoices :: DPTable -> Int -> ValveIndex -> ValveMap -> [TableCell]
openChoices table row valveI vm
  | rate == 0 = []
  | otherwise = map (openValve valveI) choices
  where
    Valve rate adj = fromJust $ Map.lookup valveI vm
    minute = minuteForRow row
    turn = turnForRow row
    -- TODO update doc about row - 3 rather than 4 to get latest pressure!
    -- prevWithCurPressure vi
    --   | isReachable (table M.! (row - 4, vi)) = amendIfReachable (\(p, r) -> (p + r * turn, r + rate)) $ table M.! (row - 3, vi)
    --   | otherwise = Nothing
    -- openCurValve
    --   | isReachable (table M.! (row - 2, valveI)) = amendIfReachable (\(p, r) -> (p + r * turn, r + rate)) $ table M.! (row - 1, valveI)
    --   | otherwise = Nothing
    openedByOther vi = alreadyOpen valveI $ table M.! (row - 1, vi)
    prevWithCurPressure vi
      | openedByOther valveI = Nothing
      | otherwise = amendIfReachable (\(p, r) -> (p + r, r + rate)) $ table M.! (row - 4, vi)
    openCurValve -- TODO order
      | not (openedByOther valveI) = amendIfReachable (\(p, r) -> (p + r, r + rate)) $ table M.! (row - 2, valveI)
      | otherwise = Nothing
    adjChoices
      | minute <= 2 = []
      | otherwise = map prevWithCurPressure adj
    choices = filter (not . alreadyOpen valveI) $ catMaybes $ [openCurValve | minute > 1] ++ adjChoices

dp :: Int -> Int -> ValveMap -> DPTable -> Int -> Int -> DPTable
dp limit maxValveI vm table row valveI
  | minute > limit + 1 = table -- Finished bottom row.
  | valveI > maxValveI = recurse table (row + 1) 1 -- Finished a row; move onto next.
  | otherwise = recurse table' row (valveI + 1)
  where
    minute = minuteForRow row
    recurse = dp limit maxValveI vm
    maxByPotentialPressure = maximumBy $ comparing (potentialPressure (limit - minute))
    -- TODO only calculate prevBest once
    -- prevBest
    --   | row > 1 = maxByPotentialPressure $ V.toList $ M.getRow (row - 1) table
    --   | otherwise = emptyCell
    nocs = nonOpenChoices table row valveI vm
    ocs = openChoices table row valveI vm
    choices = nocs ++ ocs
    table'
      -- \| minute == limit + 1 = M.setElem prevBest (row, valveI) table
      | null choices = table
      | otherwise = M.setElem (maxByPotentialPressure choices) (row, valveI) table

makeTable :: Int -> ValveMap -> Int -> DPTable
makeTable limit vm startValveIndex = dp limit nValves vm table 1 1
  where
    table = M.matrix ((limit + 1) * 2) nValves firstReachableOnly
    firstReachableOnly (row, col)
      | row <= 2 && col == startValveIndex = emptyCell
      | otherwise = Unreachable
    nValves = Map.size vm

valveMap :: ValveMap
valveMap =
  Map.fromList
    [ (1, Valve 0 [4, 9, 2]),
      (2, Valve 13 [3, 1]),
      (3, Valve 2 [4, 2]),
      (4, Valve 20 [3, 1, 5]),
      (5, Valve 3 [6, 4]),
      (6, Valve 0 [5, 7]),
      (7, Valve 0 [6, 8]),
      (8, Valve 22 [7]),
      (9, Valve 0 [1, 10]),
      (10, Valve 21 [9])
    ]
