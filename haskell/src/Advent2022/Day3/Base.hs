module Advent2022.Day3.Base (priority, commonItemAll) where

import Data.Char (ord)
import qualified Data.Set as Set

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

commonItems :: String -> String -> String
commonItems s1 = filter (`Set.member` s1set)
  where
    s1set = Set.fromList s1

commonItemsAll :: [String] -> String
commonItemsAll = foldr1 commonItems

-- This finds the first common item if there are several, which is fine:
-- we're given the constraint that there's always exactly one.
commonItemAll :: [String] -> Char
commonItemAll = head . commonItemsAll
