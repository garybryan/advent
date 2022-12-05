module Advent2022.Day03.Base (priority, commonItemAll) where

import Data.Char (isAsciiLower, isAsciiUpper, ord)
import qualified Data.Set as Set

orda :: Int
orda = ord 'a'

ordA :: Int
ordA = ord 'A'

priority :: Char -> Int
priority c
  | isAsciiUpper c = ord c - ordA + 27
  | isAsciiLower c = ord c - orda + 1
  | otherwise = error $ "Invalid character: " ++ show c

commonItems :: String -> String -> String
commonItems s1 = filter (`Set.member` s1set)
  where
    s1set = Set.fromList s1

commonItemsAll :: [String] -> String
commonItemsAll = foldr1 commonItems

-- This finds the first common item if there are several, and raises an error if there are none,
-- which is fine: we're given the constraint that there's always exactly one.
commonItemAll :: [String] -> Char
commonItemAll = head . commonItemsAll
