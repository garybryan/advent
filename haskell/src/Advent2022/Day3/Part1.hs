module Advent2022.Day3.Part1 (commonCompartmentItemPriority, commonCompartmentItemsPriority, run) where

import Advent2022.Day3.Base

-- An odd-sized rucksack will have a bigger second compartment with this logic,
-- but the question says that all have even length (same number in each compartment).
splitRucksack :: String -> (String, String)
splitRucksack s = splitAt (length s `div` 2) s

commonCompartmentItem :: (String, String) -> Char
commonCompartmentItem (s1, s2) = commonItemAll [s1, s2]

commonCompartmentItemPriority :: String -> Int
commonCompartmentItemPriority = priority . commonCompartmentItem . splitRucksack

commonCompartmentItemsPriority :: [String] -> Int
commonCompartmentItemsPriority = sum . map commonCompartmentItemPriority

run :: [String] -> String
run ls = "Total priority of common items in compartments: " ++ show (commonCompartmentItemsPriority ls)
