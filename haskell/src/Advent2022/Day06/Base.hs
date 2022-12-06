module Advent2022.Day06.Base (indexAfterUniquePrefix, parseLines) where

import Data.Maybe
import qualified Data.Set as Set

numUniques :: (Ord a) => [a] -> Int
numUniques = Set.size . Set.fromList

uniquePrefix :: (Ord a) => Int -> [a] -> Bool
uniquePrefix n xs = n == numUniques (take n xs)

indexAfterUniquePrefix :: (Ord a) => Int -> [a] -> Int
indexAfterUniquePrefix n = go 0
  where
    go :: (Ord a) => Int -> [a] -> Int
    go _ [] = -1
    go i xs'
      | uniquePrefix n xs' = i + n
      | otherwise = go (i + 1) (tail xs')

-- There's only one line to parse, and it's already a string: easiest parse yet!
parseLines :: [String] -> String
parseLines ss = fromMaybe (error "Empty file") (listToMaybe ss)
