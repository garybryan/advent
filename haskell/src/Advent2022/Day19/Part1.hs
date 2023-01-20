module Advent2022.Day19.Part1
  ( qualityLevel,
    qualityLevels,
    run,
  )
where

import Advent2022.Day19.Base
import Advent2022.Day19.Parsing

qualityLevel :: Blueprint -> Int
qualityLevel (i, costs) = i * bnbFromInitial 24 costs

qualityLevels :: [Blueprint] -> Int
qualityLevels = sum . map qualityLevel

run :: [String] -> String
run ss = "Result: " ++ show (qualityLevels $ parseLines ss)
