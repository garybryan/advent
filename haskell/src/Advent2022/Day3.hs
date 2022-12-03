module Advent2022.Day3 (priority, commonItemPriority, commonItemAll, commonItemsPriority, run) where

import Data.Char (ord)
import qualified Data.Set as Set
import Lib.Read (readLines)

orda :: Int
orda = ord 'a'

ordz :: Int
ordz = ord 'z'

ordA :: Int
ordA = ord 'A'

ordZ :: Int
ordZ = ord 'Z'

priority :: Char -> Int
priority c
  | o >= ordA && o <= ordZ = o - ordA + 27
  | o >= orda && o <= ordz = o - orda + 1
  | otherwise = error $ "Invalid character: " ++ show c
  where
    o = ord c

-- An odd-sized rucksack will have a bigger second compartment with this logic,
-- but the question says that all have even length (same number in each compartment)
splitRucksack :: String -> (String, String)
splitRucksack s = splitAt (length s `div` 2) s

commonItems :: String -> String -> String
commonItems s1 = filter (`Set.member` s1set)
  where
    s1set = Set.fromList s1

commonItemsAll :: [String] -> String
commonItemsAll = foldr1 commonItems

-- This finds the first common item if there are several, which again is fine:
-- we're given the constraint that there's always exactly one.
commonItemAll :: [String] -> Char
commonItemAll = head . commonItemsAll

commonItemPair :: (String, String) -> Char
commonItemPair (s1, s2) = commonItemAll [s1, s2]

commonItemPriority :: String -> Int
commonItemPriority = priority . commonItemPair . splitRucksack

commonItemsPriority :: [String] -> Int
commonItemsPriority = sum . map commonItemPriority

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let totalPriority = commonItemsPriority fileLines
  putStrLn $ "Part 1: Total priority of common items: " ++ show totalPriority
