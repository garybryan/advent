module Advent2022.Day05.Part1 (move, moveSeveral, applyMovesToLinesSeveral, run) where

import Advent2022.Day05.Base
import qualified Data.Dequeue as DQ
import Data.Maybe
import qualified Data.Vector as V

move :: Int -> Int -> Stacks -> Stacks
move from to ss = ss V.// [(from0, newFrom), (to0, newTo)]
  where
    from0 = from - 1
    to0 = to - 1
    (c, newFrom) = fromMaybe (error "Empty stack!") (DQ.popBack $ ss V.! from0)
    newTo = DQ.pushBack (ss V.! to0) c

-- TODO there'll be a way to do this and other (n-1) relations with fold etc. rather than explicit recursion
moveSeveral :: Move -> Stacks -> Stacks
moveSeveral (n, from, to) ss
  | n == 0 = ss
  | otherwise = moveSeveral (n - 1, from, to) (move from to ss)

applyMovesToLinesSeveral :: [String] -> Stacks
applyMovesToLinesSeveral = applyMovesToLines moveSeveral

topCratesFromLines :: [String] -> String
topCratesFromLines = topCrates . applyMovesToLinesSeveral

run :: [String] -> String
run ls = "Final top crates from moving one at a time: " ++ show (topCratesFromLines ls)
