module Advent2021.Day3.Base
  ( binStrToInt,
    bitFreqs,
    mostFrequent,
    gammaRate,
    epsilonRate,
    powerConsumption,
    powerConsumptionFromLines,
    run,
  )
where

import Data.Bits
import Lib.Read (readLines)

binCharToDigit :: Char -> Int
binCharToDigit c
  | c == '0' = 0
  | c == '1' = 1
  | otherwise = error $ "Invalid binary digit: " ++ show c

binDigitsToInt :: [Int] -> Int
binDigitsToInt = foldl1 (\acc n -> acc * 2 + n)

binStrToInt :: String -> Int
binStrToInt = binDigitsToInt . map binCharToDigit

-- Make a blank frequency list for `nBits` bits.
initialFreqs :: Int -> [Int]
initialFreqs nBits = replicate nBits 0

-- Get the frequencies of 1 for each bit in a list of numbers.
-- For each number, it builds a new list by repeatedly dividing the number by two
-- to get the next bit (the remainder) and adding that to the previous frequency for that position.
-- Since Haskell works efficiently by concatenating to the head of a list rather than the tail,
-- and dividing a number by 2 gives its least significant bit, the list is processed in reverse order.
-- least significant bit first. It's then returned in "normal" order, just to make it easier to reason with
-- and to allow reuse of some of the string-to-number conversion code.
-- This is clearly less efficient than it would be using mutability, since it builds a new list each time.
bitFreqs :: Int -> [Int] -> [Int]
bitFreqs nBits = reverse . foldr aux (initialFreqs nBits)
  where
    aux :: Int -> [Int] -> [Int]
    aux _ [] = []
    aux x (f : fs) = r + f : aux q fs
      where
        (q, r) = x `divMod` 2

-- Take a frequency list (result of `bitFreqs`), given the total number of numbers,
-- and return a list of the most frequent bit value (0 or 1) for each position.
-- We're told that there will always be a most common bit, so we don't have to worry about an equal case.
mostFrequentInFreqs :: Int -> [Int] -> [Int]
mostFrequentInFreqs n = map $ fromEnum . (> (n `div` 2))

-- TODO instead of getting the lengths of xs, could calculate it on the first pass through the list (bitFreqs),
-- to keep things lazy?
mostFrequent :: Int -> [Int] -> [Int]
mostFrequent nBits xs = mostFrequentInFreqs nNums (bitFreqs nBits xs)
  where
    nNums = length xs

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
