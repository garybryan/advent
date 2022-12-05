module Advent2022.Day05.Part1 (Stack, Stacks, move, moveSeveral, parseMove, parseStackLine, parseStackLines, applyMoves, finalStacksFromLines, topCrates, run) where

import Advent2022.Day05.Base
import Data.Char
import qualified Data.Dequeue as DQ
import Data.Foldable (toList)
import Data.List.Split
import Data.Maybe
import qualified Data.Vector as V

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
parseStackLine s = V.fromList $ map groupToStack (chunksOf 4 s)

-- TODO less horrible implementation than converting to and from lists
combineStacks :: Stack -> Stack -> Stack
combineStacks s1 s2 = DQ.fromList $ toList s2 ++ toList s1

-- TODO zipwidth will be shortest rather than longest
combineStackVectors :: Stacks -> Stacks -> Stacks
combineStackVectors = V.zipWith combineStacks

readNumbers :: String -> [Int]
readNumbers = map read . filter (all isDigit) . splitOn " "

parseMove :: String -> (Int, Int, Int)
parseMove s = (head nums, nums !! 1, nums !! 2)
  where
    nums = readNumbers s

-- Like uncurry but for a triplet
moveFunc :: (Int, Int, Int) -> (Stacks -> Stacks)
moveFunc (num, from, to) = moveSeveral num from to

-- TODO make this work properly with uneven lines
parseStackLines :: [String] -> Stacks
parseStackLines = foldr1 combineStackVectors . map parseStackLine

-- File format is stack lines then blank line then movement lines
-- TODO common splitIn2 / split by char function
splitInput :: [String] -> ([String], [String])
splitInput ss = (init $ head ssSplit, last ssSplit)
  where
    ssSplit = split (whenElt (== "")) ss

applyMoves :: Stacks -> [(Int, Int, Int)] -> Stacks
applyMoves ss ms = moveFuncs ss
  where
    moveFuncs = foldl1 (flip (.)) (map moveFunc ms)

finalStacksFromLines :: [String] -> Stacks
finalStacksFromLines s = ss `applyMoves` ms
  where
    (stackLines, moveLines) = splitInput s
    ss = parseStackLines stackLines
    ms = map parseMove moveLines

topCrate :: Stack -> Char
topCrate = fromMaybe ' ' . DQ.last

topCrates :: Stacks -> String
topCrates = V.toList . V.map topCrate

finalTopCratesFromLines :: [String] -> String
finalTopCratesFromLines = topCrates . finalStacksFromLines

run :: [String] -> String
run ls = "Result: " ++ show (finalTopCratesFromLines ls)
