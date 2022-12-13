module Advent2022.Day01 (run) where

import Lib.Top (topK)
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

getMax3Total :: [Int] -> Int
getMax3Total = sum . topK 3

run :: [String] -> String
run ls = "Part 1: Max calories for one elf: " ++ show maxCalories ++ "; Part 2: Total of top 3 max: " ++ show totalTop3
  where
    calories = getCaloriesByElf $ map readLine ls
    maxCalories = getMaxCalories calories
    totalTop3 = getMax3Total calories
