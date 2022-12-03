module Advent2021.Day3.Part2
  ( matchBit,
    filterOnBit
  )
where

import Advent2021.Day3.Base
import Data.Bits
import Lib.Read (readLines)

-- Gives whether a certain bit (zero-indexed, starting from LSB) of the number has a certain value.
matchBit :: Int -> Int -> Int -> Bool
matchBit i v n = n `shiftR` i .&. 1 == v

filterOnBit :: Int -> Int -> [Int] -> [Int]
filterOnBit i v = filter $ matchBit i v
