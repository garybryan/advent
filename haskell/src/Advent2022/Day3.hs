module Advent2022.Day3 (priority) where

import Data.Char (ord)

orda :: Int
orda = ord 'a'

ordz :: Int
ordz = ord 'z'

ordA :: Int
ordA = ord 'A'

ordZ :: Int
ordZ = ord 'Z'

priority :: Char -> Int
priority c
  | o >= ordA && o <= ordZ = o - ordA + 27
  | o >= orda && o <= ordz = o - orda + 1
  | otherwise = error $ "Invalid character: " ++ show c
  where
    o = ord c
