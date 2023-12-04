module Advent2023.Day01.Part1
  ( digits,
    run,
  )
where

import Advent2023.Day01.Base
import Data.Char (digitToInt, isDigit)

digits :: String -> [Int]
digits = map digitToInt . filter isDigit

run :: [String] -> String
run ss = "Sum of calibration values: " ++ show (calibrationValueSum digits ss)
