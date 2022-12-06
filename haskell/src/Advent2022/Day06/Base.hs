module Advent2022.Day06.Base (indexAfterUniquePrefix) where

import qualified Data.List as List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . List.nub

uniquePrefix :: (Eq a) => Int -> [a] -> Bool
uniquePrefix n xs = n == numUniques (take n xs)

indexAfterUniquePrefix :: (Eq a) => Int -> [a] -> Int
indexAfterUniquePrefix n = go 0
  where
    go :: (Eq a) => Int -> [a] -> Int
    go _ [] = -1
    go i xs'
      | uniquePrefix n xs' = i + n
      | otherwise = go (i + 1) (tail xs')
