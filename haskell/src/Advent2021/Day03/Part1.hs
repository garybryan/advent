module Advent2021.Day03.Part1
  ( gammaRate,
    epsilonRate,
    powerConsumption,
    powerConsumptionFromLines,
    run,
  )
where

import Advent2021.Day03.Base
import Data.Bits

gammaRate :: Int -> [Int] -> Int
gammaRate nBits = binDigitsToInt . mostFrequent nBits

-- Get epsilon rate from gamma rate by flipping the bits.
-- Needs the total number of bits to remove trailing 1s after the most significant.
-- Of course, we could also just do the opposite of `binDigitsToInt` with the frequency array,
-- but this is a little less work.
epsilonRate :: Int -> Int -> Int
epsilonRate nBits = (.&. mask) . complement
  where
    mask = 2 ^ nBits - 1

powerConsumption :: Int -> [Int] -> Int
powerConsumption nBits xs = g * epsilonRate nBits g
  where
    g = gammaRate nBits xs

powerConsumptionFromLines :: [String] -> Int
powerConsumptionFromLines ls = powerConsumption (numBitsNeeded ls) (intLines ls)

run :: [String] -> String
run ls = "Power consumption: " ++ show (powerConsumptionFromLines ls)
