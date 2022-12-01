import qualified Data.Heap as Heap
import Text.Read (readMaybe)

import Utils.Runner

readLine :: String -> Maybe Int
readLine l = readMaybe l

getCaloriesByElf :: [Maybe Int] -> [Int]
getCaloriesByElf [] = [0]
getCaloriesByElf (Nothing:xs) = 0 : getCaloriesByElf xs
getCaloriesByElf (Just x:xs) = (x + head rest) : tail rest
  where rest = getCaloriesByElf xs

-- Standard k-max elements implementation using a min-heap to store the k largest values.
getMaxK :: Int -> [Int] -> [Int]
getMaxK k xs = aux k (Heap.empty :: Heap.MinHeap Int) xs
  where 
    aux k h [] = Heap.toList h
    aux k h (x:xs) = aux k (if Heap.size h' > k then (Heap.drop 1 h') else h') xs
      where h' = Heap.insert x h

main = do
  caloriesPerElf <- run (getCaloriesByElf . map readLine)
  let maxCalories = maximum caloriesPerElf
  putStrLn ("Part 1: Max calories for one elf: " ++ (show maxCalories))
  let top3MaxCalories = getMaxK 3 caloriesPerElf
  putStrLn ("Part 2: Top 3 calories per elf: " ++ (show top3MaxCalories))
  let totalTop3MaxCalories = sum top3MaxCalories
  putStrLn ("Part 2: Top 3 calories per elf: " ++ (show totalTop3MaxCalories))
