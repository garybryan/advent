module Advent2021.Day3.Part1
  ( gammaRate,
    epsilonRate,
    powerConsumption,
    powerConsumptionFromLines,
    run,
  )
where

import Advent2021.Day3.Base
import Data.Bits
import Lib.Read (readLines)

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

numBitsNeeded :: [String] -> Int
numBitsNeeded = length . head

powerConsumptionFromLines :: [String] -> Int
powerConsumptionFromLines ls = powerConsumption (numBitsNeeded ls) (map binStrToInt ls)

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  putStrLn $ "Power consumption: " ++ show (powerConsumptionFromLines fileLines)
