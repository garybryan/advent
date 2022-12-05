module Advent2022.Day05.Part1 (move, moveSeveral, applyMoves, applyMovesToLines, run) where

import Advent2022.Day05.Base
import qualified Data.Dequeue as DQ
import Data.Maybe
import qualified Data.Vector as V

-- TODO could reuse moveSeveralAtOnce from Part 2 to do this: just always call with 1.
move :: Int -> Int -> Stacks -> Stacks
move from to ss = ss V.// [(from0, newFrom), (to0, newTo)]
  where
    from0 = from - 1
    to0 = to - 1
    (c, newFrom) = fromMaybe (error "Empty stack!") (DQ.popBack $ ss V.! from0)
    newTo = DQ.pushBack (ss V.! to0) c

-- TODO there'll be a way to do this and other (n-1) relations with fold etc. rather than explicit recursion
moveSeveral :: Int -> Int -> Int -> Stacks -> Stacks
moveSeveral n from to ss
  | n == 0 = ss
  | otherwise = moveSeveral (n - 1) from to (move from to ss)

-- Like uncurry but for a triplet
moveFunc :: Move -> (Stacks -> Stacks)
moveFunc (num, from, to) = moveSeveral num from to

applyMoves :: Stacks -> [Move] -> Stacks
applyMoves ss ms = moveFuncs ss
  where
    moveFuncs = foldl1 (flip (.)) (map moveFunc ms)

applyMovesToLines :: [String] -> Stacks
applyMovesToLines s = ss `applyMoves` ms
  where
    (stackLines, moveLines) = splitInput s
    ss = parseStackLines stackLines
    ms = map parseMove moveLines

topCratesFromLines :: [String] -> String
topCratesFromLines = topCrates . applyMovesToLines

run :: [String] -> String
run ls = "Final top crates from moving one at a time: " ++ show (topCratesFromLines ls)
