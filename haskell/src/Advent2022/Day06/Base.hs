module Advent2022.Day06.Base (indexAfterUniquePrefix, indexAfterUniquePrefixSliding, parseLines) where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

{-
  Basic approach: at each index, check if the prefix starting at that index is
  unique.

  Using sets to check uniqueness takes the complexity down from O(n*k^2) to O(n*k)
  (or with Haskell's tree-based sets, O(n * (k log k)))
-}

numUniques :: (Ord a) => [a] -> Int
numUniques = Set.size . Set.fromList

uniquePrefix :: (Ord a) => Int -> [a] -> Bool
uniquePrefix k xs = k == numUniques (take k xs)

indexAfterUniquePrefix :: (Ord a) => Int -> [a] -> Int
indexAfterUniquePrefix k xs = go 0 xs
  where
    len = length xs
    go :: (Ord a) => Int -> [a] -> Int
    go i xs'
      | i > len - k = -1
      | uniquePrefix k xs' = i + k
      | otherwise = go (i + 1) (tail xs')

{-
  Sliding window approach: Find the first window with all unique characters of
  length k.

  Keep a map of the index of the last ocurrence of each character so far.

  On each iteration, expand the window's end by 1, and then check if the new
  character has already occurred after the start of the window.
  If so, shrink the window so it starts after the last occurrence of the
  character.

  This means that the window always contains unique characters.
  As soon as the window size reaches k, return the end index plus one.

  This brings the time complexity down to O(n) with hash maps, or O(n * log k)
  with Haskell's tree-based maps.
-}

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

slide :: (Ord a) => Int -> Map.Map a Int -> Int -> [(Int, a)] -> Int
slide _ _ _ [] = -1
slide k indices start ((end, endItem) : xs)
  | end + 1 - start' == k = end + 1
  | otherwise = slide k indices' start' xs
  where
    start' = max start (Map.findWithDefault (-1) endItem indices + 1)
    indices' = Map.insert endItem end indices

indexAfterUniquePrefixSliding :: (Ord a) => Int -> [a] -> Int
indexAfterUniquePrefixSliding k xs = slide k Map.empty 0 (enumerate xs)

-- There's only one line to parse, and it's already a string: easiest parse yet!
parseLines :: [String] -> String
parseLines ss = fromMaybe (error "Empty file") (listToMaybe ss)
