module Advent2022.Day05.Base (Move, Stack, Stacks, splitInput, parseMove, parseStack, parseStacks, topCrates, applyMovesToLines) where

import Data.Char
import qualified Data.Dequeue as DQ
import Data.Foldable (toList)
import Data.List (isPrefixOf)
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

parseStack :: String -> Stacks
parseStack s = V.fromList $ map groupToStack (chunksOf 4 s)

-- Add the elements from the first stack, from the front, onto the top of the second,
-- so that repeated application results in stacks with the first elements at the top (back).
combineStacks :: Stack -> Stack -> Stack
combineStacks s1 s2 = foldl DQ.pushBack s2 (toList s1)

-- zipWith works here because input lines all have the same length:
-- empty stacks from the right are padded with whitespace.
combineStackVectors :: Stacks -> Stacks -> Stacks
combineStackVectors = V.zipWith combineStacks

-- File format is stack lines then blank line then movement lines.
splitInput :: [String] -> ([String], [String])
splitInput ss = (stackLines, actionLines)
  where
    (stackLines, rest) = break (isPrefixOf " 1") ss
    actionLines = dropWhile (not . isPrefixOf "move") rest

readNumbers :: String -> [Int]
readNumbers = map read . filter (all isDigit) . splitOn " "

parseMove :: String -> Move
parseMove s = (head nums, nums !! 1, nums !! 2)
  where
    nums = readNumbers s

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseStacks :: [String] -> Stacks
parseStacks = foldr1 combineStackVectors . map parseStack

parseInput :: [String] -> (Stacks, [Move])
parseInput ss = (parseStacks sts, parseMoves ms)
  where
    (sts, ms) = splitInput ss

topCrate :: Stack -> Char
topCrate = fromMaybe ' ' . DQ.last

topCrates :: Stacks -> String
topCrates = V.toList . V.map topCrate

composeMoves :: (Move -> Stacks -> Stacks) -> [Move] -> (Stacks -> Stacks)
composeMoves f ms = foldl1 (flip (.)) (map f ms)

applyMovesToLines :: (Move -> Stacks -> Stacks) -> [String] -> Stacks
applyMovesToLines f ss = applyMoves sts
  where
    (sts, ms) = parseInput ss
    applyMoves = composeMoves f ms
