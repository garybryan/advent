module Advent2021.Day3.Base (binStrToInt, bitFreqs) where

binCharToInt :: Char -> Int
binCharToInt c
  | c == '0' = 0
  | c == '1' = 1
  | otherwise = error $ "Invalid binary digit: " ++ show c

binStrToInt :: String -> Int
binStrToInt = foldl1 (\a n -> a * 2 + n) . map binCharToInt

-- Make a blank frequency list for `k` bits.
initialFreqs :: Int -> [Int]
initialFreqs k = replicate k 0

-- Get the frequencies of 1 for each bit in a list of numbers.
-- For each number, it builds a new list by repeatedly dividing the number by two
-- to get the next bit (the remainder) and adding that to the previous frequency for that position.
-- Since Haskell works efficiently by concatenating to the head of a list rather than the tail,
-- and dividing a number by 2 gives its least significant bit, the list is processed in reverse order.
-- least significant bit first. It's then returned in "normal" order, just to make it easier to reason with.
-- This is clearly less efficient than it would be using mutability, since it builds a new list each time.
bitFreqs :: Int -> [Int] -> [Int]
bitFreqs k = reverse . foldr aux (initialFreqs k)
  where
    aux :: Int -> [Int] -> [Int]
    aux _ [] = []
    aux x (f : fs) = r + f : aux q fs
      where
        (q, r) = x `divMod` 2
