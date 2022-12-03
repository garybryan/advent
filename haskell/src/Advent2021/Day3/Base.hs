module Advent2021.Day3.Base (binStrToInt) where

binCharToInt :: Char -> Int
binCharToInt c
  | c == '0' = 0
  | c == '1' = 1
  | otherwise = error $ "Invalid binary digit: " ++ show c

binStrToInt :: String -> Int
binStrToInt = foldl1 (\a n -> a * 2 + n) . map binCharToInt
