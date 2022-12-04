module Advent2021.Day3.Part2
  ( matchBit,
    filterOnBit,
    mostCommonInPos,
    filterToMostCommonInPos,
    filterToLeastCommonInPos,
    oxygenRating,
    co2Rating,
    lifeSupportRating,
    run,
  )
where

import Advent2021.Day3.Base
import Data.Bits

-- Gives whether a certain bit (zero-indexed, starting from LSB) of the number has a certain value.
matchBit :: Int -> Int -> Int -> Bool
matchBit i v n = n `shiftR` i .&. 1 == v

filterOnBit :: Int -> Int -> [Int] -> [Int]
filterOnBit i v = filter $ matchBit i v

onesInPos :: Int -> [Int] -> Int
onesInPos i = length . filterOnBit i 1

mostCommonInPos :: Int -> [Int] -> Int
mostCommonInPos i xs = fromEnum $ onesInPos i xs >= -(length xs `div` (-2))

filterToMostCommonInPos :: Int -> [Int] -> [Int]
filterToMostCommonInPos i xs = filterOnBit i (mostCommonInPos i xs) xs

filterToLeastCommonInPos :: Int -> [Int] -> [Int]
filterToLeastCommonInPos i xs = filterOnBit i (mostCommonInPos i xs `complementBit` 0) xs

filterToOne :: Int -> (Int -> [Int] -> [Int]) -> [Int] -> Int
filterToOne _ _ [x] = x
filterToOne i filterFn xs = filterToOne (i - 1) filterFn (filterFn i xs)

oxygenRating :: Int -> [Int] -> Int
oxygenRating numBits = filterToOne (numBits - 1) filterToMostCommonInPos

co2Rating :: Int -> [Int] -> Int
co2Rating numBits = filterToOne (numBits - 1) filterToLeastCommonInPos

lifeSupportRating :: Int -> [Int] -> Int
lifeSupportRating numBits xs = oxygenRating numBits xs * co2Rating numBits xs

lifeSupportRatingFromLines :: [String] -> Int
lifeSupportRatingFromLines ls = lifeSupportRating (numBitsNeeded ls) (intLines ls)

run :: [String] -> String
run ls = "Life support rating: " ++ show (lifeSupportRatingFromLines ls)
