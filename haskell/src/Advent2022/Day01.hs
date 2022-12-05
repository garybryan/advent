module Advent2022.Day01 (run) where

import qualified Data.Heap as H
import Data.Maybe
import Text.Read (readMaybe)

readLine :: String -> Maybe Int
readLine = readMaybe

getCaloriesByElf :: [Maybe Int] -> [Int]
getCaloriesByElf [] = [0]
getCaloriesByElf (Nothing : xs) = 0 : getCaloriesByElf xs
getCaloriesByElf (Just x : xs) = (x + head rest) : tail rest
  where
    rest = getCaloriesByElf xs

getMaxCalories :: [Int] -> Int
getMaxCalories = maximum

type IntHeap = H.MinHeap Int

-- Standard k-max elements implementation using a min-heap to store the k largest values.
-- Sorting the list and returning the top k values is O(n log n), but this is O(n log k)
-- since it only stores k elements on the heap, and even better in the average case of a
-- non-ascending list since heap operations are only done when needed.
getMaxK :: Int -> [Int] -> [Int]
getMaxK k xs = H.toList $ foldl updateHeap initialHeap rest
  where
    initialHeap = H.fromList (take k xs) :: IntHeap
    rest = drop k xs
    updateHeap h x
      | x > fromMaybe (error "Empty heap") (H.viewHead h) = H.insert x (H.drop 1 h)
      | otherwise = h

getMax3Total :: [Int] -> Int
getMax3Total = sum . getMaxK 3

run :: [String] -> String
run ls = "Part 1: Max calories for one elf: " ++ show maxCalories ++ "; Part 2: Total of top 3 max: " ++ show totalTop3
  where
    calories = getCaloriesByElf $ map readLine ls
    maxCalories = getMaxCalories calories
    totalTop3 = getMax3Total calories
