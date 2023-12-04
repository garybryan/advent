module Advent2023.Day01.Base
  ( calibrationValue,
    calibrationValueSum,
  )
where

type DigitsFunc = (String -> [Int])

calibrationValue :: DigitsFunc -> String -> Int
calibrationValue digitsFunc s = head sDigits * 10 + last sDigits
  where
    sDigits = digitsFunc s

calibrationValueSum :: DigitsFunc -> [String] -> Int
calibrationValueSum digitsFunc = sum . map (calibrationValue digitsFunc)
