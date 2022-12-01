module Advent2022.Day1 (main) where

import qualified Data.Heap as Heap
import Text.Read (readMaybe)

import Lib.Runner

readLine :: String -> Maybe Int
readLine l = readMaybe l

getCaloriesByElf :: [Maybe Int] -> [Int]
getCaloriesByElf [] = [0]
getCaloriesByElf (Nothing:xs) = 0 : getCaloriesByElf xs
getCaloriesByElf (Just x:xs) = (x + head rest) : tail rest
  where rest = getCaloriesByElf xs

-- Standard k-max elements implementation using a min-heap to store the k largest values.
getMaxK :: Int -> [Int] -> [Int]
getMaxK k xs = aux (Heap.empty :: Heap.MinHeap Int) xs
  where 
    aux h [] = Heap.toList h
    aux h (x:xs') = aux (if Heap.size h' > k then (Heap.drop 1 h') else h') xs'
      where h' = Heap.insert x h

main :: FilePath -> IO ()
main filePath = do
  caloriesPerElf <- run filePath (getCaloriesByElf . map readLine)
  let maxCalories = maximum caloriesPerElf
  putStrLn ("Part 1: Max calories for one elf: " ++ (show maxCalories))
  let top3MaxCalories = getMaxK 3 caloriesPerElf
  putStrLn ("Part 2: Top 3 calories per elf: " ++ (show top3MaxCalories))
  let totalTop3MaxCalories = sum top3MaxCalories
  putStrLn ("Part 2: Top 3 calories per elf: " ++ (show totalTop3MaxCalories))
