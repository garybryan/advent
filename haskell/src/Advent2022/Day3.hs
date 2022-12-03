module Advent2022.Day3 (priority, commonCompartmentItemPriority, commonCompartmentItemsPriority, priorityGroupsOf3, run) where

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

commonCompartmentItem :: (String, String) -> Char
commonCompartmentItem (s1, s2) = commonItemAll [s1, s2]

commonCompartmentItemPriority :: String -> Int
commonCompartmentItemPriority = priority . commonCompartmentItem . splitRucksack

commonCompartmentItemsPriority :: [String] -> Int
commonCompartmentItemsPriority = sum . map commonCompartmentItemPriority

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf k l = take k l : groupsOf k (drop k l)

priorityGroupsOf3 :: [String] -> Int
priorityGroupsOf3 = sum . map (priority . commonItemAll) . groupsOf 3

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath

  let totalPriority = commonCompartmentItemsPriority fileLines
  putStrLn $ "Part 1: Total priority of common items in compartments: " ++ show totalPriority

  let priority3s = priorityGroupsOf3 fileLines
  putStrLn $ "Part 2: Total priority of common items in groups of 3: " ++ show priority3s
