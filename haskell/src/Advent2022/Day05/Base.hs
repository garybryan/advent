module Advent2022.Day05.Base (Move, Stack, Stacks, splitInput, parseMove, parseStackLine, parseStackLines, topCrates) where

import Data.Char
import qualified Data.Dequeue as DQ
import Data.Foldable (toList)
import Data.List.Split
import Data.Maybe
import qualified Data.Vector as V

type Move = (Int, Int, Int)

-- A deque can be (and often is, e.g. in Python) used as a queue or a stack.
-- Queues are bound to come up sooner or later so let's just learn one lib instead of two!
type Stack = DQ.BankersDequeue Char

-- TODO do we really need Vector when we're rebuilding anyway? Might as well use a plain list and range.
type Stacks = V.Vector Stack

-- The parsing for this one is a pain: really not the nicest format!
-- Just split the line into groups of (up to) 4 characters then parse [c] as an entry,
-- and blank space as no entry, for that position.
-- The first line might have no entry for the last stack(s), so successive applications need to handle
-- adding them to the accumulated list.
--
isSpaces :: String -> Bool
isSpaces = all (== ' ')

groupToStack :: String -> Stack
groupToStack s
  | isSpaces s = DQ.empty
  | otherwise = DQ.fromList [s !! 1] -- quick 'n' dirty, but the 2nd char of the string is what we want

parseStackLine :: String -> Stacks
parseStackLine s = V.fromList $ map groupToStack (chunksOf 4 s)

-- TODO less horrible implementation than converting to and from lists.
-- We just want to append an item onto the accumulating stack.
combineStacks :: Stack -> Stack -> Stack
combineStacks s1 s2 = DQ.fromList $ toList s2 ++ toList s1

-- TODO zipWith will be shortest rather than longest
combineStackVectors :: Stacks -> Stacks -> Stacks
combineStackVectors = V.zipWith combineStacks

readNumbers :: String -> [Int]
readNumbers = map read . filter (all isDigit) . splitOn " "

parseMove :: String -> Move
parseMove s = (head nums, nums !! 1, nums !! 2)
  where
    nums = readNumbers s

-- TODO make this work properly with uneven lines.
-- This happens to work because the given data happens to have a max-height last stack,
-- but it should be flexible enough to work without.
parseStackLines :: [String] -> Stacks
parseStackLines = foldr1 combineStackVectors . map parseStackLine

-- File format is stack lines then blank line then movement lines.
-- TODO common splitIn2 / split by char function as it keeps coming up.
splitInput :: [String] -> ([String], [String])
splitInput ss = (init $ head ssSplit, last ssSplit)
  where
    ssSplit = split (whenElt (== "")) ss

topCrate :: Stack -> Char
topCrate = fromMaybe ' ' . DQ.last

topCrates :: Stacks -> String
topCrates = V.toList . V.map topCrate
