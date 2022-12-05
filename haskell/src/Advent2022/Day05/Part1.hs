module Advent2022.Day05.Part1 (Stack, Stacks, move, moveSeveral, parseStackLine) where

import Advent2022.Day05.Base
import qualified Data.Dequeue as DQ
import Data.Maybe
import qualified Data.Vector as V
import Lib.Groups

type Stack = DQ.BankersDequeue Char

type Stacks = V.Vector Stack

-- TODO with recursion and lists rather than vector? More Haskelly
move :: Int -> Int -> Stacks -> Stacks
move from to ss = ss V.// [(from0, newFrom), (to0, newTo)]
  where
    from0 = from - 1
    to0 = to - 1
    (c, newFrom) = fromMaybe (error "Empty stack!") (DQ.popBack $ ss V.! from0)
    newTo = DQ.pushBack (ss V.! to0) c

-- TODO there'll be a way to do this with foldl etc. rather than explicit recursion
moveSeveral :: Int -> Int -> Int -> Stacks -> Stacks
moveSeveral n from to ss
  | n == 0 = ss
  | otherwise = moveSeveral (n - 1) from to (move from to ss)

-- The parsing for this one is a pain: really not the nicest format!
-- Just split the line into groups of (up to) 4 characters then parse [c] as an entry
-- and blank space as no new entry for that position.
-- The first line might have no entry for the last stack(s), so successive applications need to handle
-- adding them to the accumulated list.
isSpaces :: String -> Bool
isSpaces = all (== ' ')

groupToStack :: String -> Stack
groupToStack s
  | isSpaces s = DQ.empty
  | otherwise = DQ.fromList [s !! 1] -- quick 'n' dirty, but the 2nd char of the string is what we want

parseStackLine :: String -> Stacks
parseStackLine s = V.fromList $ map groupToStack (groupsOf 4 s)

-- run :: [String] -> String
-- run ls = "Result: " ++ show (doThing)
