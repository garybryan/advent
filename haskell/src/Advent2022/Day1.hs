module Advent2022.Day1 (run) where

import qualified Data.Heap as H
import Lib.Read (readLines)
import Text.Read (readMaybe)

readLine :: String -> Maybe Int
readLine = readMaybe

getCaloriesByElf :: [Maybe Int] -> [Int]
getCaloriesByElf [] = [0]
getCaloriesByElf (Nothing : xs) = 0 : getCaloriesByElf xs
getCaloriesByElf (Just x : xs) = (x + head rest) : tail rest
  where
    rest = getCaloriesByElf xs

-- Needed for type safety as `H.viewHead` could be Nothing for an empty heap;
-- in reality it'll never be empty, but the compiler doesn't know that!
gtIfJust :: (Ord a) => a -> Maybe a -> Bool
gtIfJust _ Nothing = False
gtIfJust x (Just y) = x > y

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
      | x `gtIfJust` H.viewHead h = H.insert x (H.drop 1 h)
      | otherwise = h

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let caloriesPerElf = getCaloriesByElf $ map readLine fileLines

  let maxCalories = maximum caloriesPerElf
  putStrLn ("Part 1: Max calories for one elf: " ++ show maxCalories)

  let top3MaxCalories = getMaxK 3 caloriesPerElf
  putStrLn ("Part 2: Top 3 calories per elf: " ++ show top3MaxCalories)

  let totalTop3MaxCalories = sum top3MaxCalories
  putStrLn ("Part 2: Total of top 3 calories per elf: " ++ show totalTop3MaxCalories)
