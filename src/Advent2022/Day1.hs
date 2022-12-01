module Advent2022.Day1 (run) where

import qualified Data.Heap as Heap
import Text.Read (readMaybe)

import Lib.Read (readLines)

readLine :: String -> Maybe Int
readLine = readMaybe

getCaloriesByElf :: [Maybe Int] -> [Int]
getCaloriesByElf [] = [0]
getCaloriesByElf (Nothing:xs) = 0 : getCaloriesByElf xs
getCaloriesByElf (Just x:xs) = (x + head rest) : tail rest
  where rest = getCaloriesByElf xs

-- Needed for type safety as `Heap.viewHead` could be Nothing for an empty heap;
-- in reality it'll never be empty, but the compiler doesn't know that!
gtIfJust :: Int -> Maybe Int -> Bool
gtIfJust _ Nothing  = False
gtIfJust x (Just y) = x > y

type IntHeap = Heap.MinHeap Int

-- Standard k-max elements implementation using a min-heap to store the k largest values.
getMaxK :: Int -> [Int] -> [Int]
getMaxK k xs = Heap.toList (foldl updateHeap initialHeap rest)
  where 
    updateHeap :: IntHeap -> Int -> IntHeap
    updateHeap h x
      | x `gtIfJust` Heap.viewHead h = Heap.insert x (Heap.drop 1 h)
      | otherwise                    = h
    initialHeap = (Heap.fromList (take k xs) :: IntHeap)
    rest = drop k xs
    

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let caloriesPerElf = getCaloriesByElf (map readLine fileLines)

  let maxCalories = maximum caloriesPerElf
  putStrLn ("Part 1: Max calories for one elf: " ++ (show maxCalories))

  let top3MaxCalories = getMaxK 3 caloriesPerElf
  putStrLn ("Part 2: Top 3 calories per elf: " ++ (show top3MaxCalories))

  let totalTop3MaxCalories = sum top3MaxCalories
  putStrLn ("Part 2: Total of top 3 calories per elf: " ++ (show totalTop3MaxCalories))
