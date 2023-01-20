module Advent2022.Day19.Part2
  ( products,
    run,
  )
where

import Advent2022.Day19.Base
import Advent2022.Day19.Parsing

products :: [Blueprint] -> Int
products = product . map (bnbFromInitial 32 . snd)

run :: [String] -> String
run ss = "Result: " ++ show (products $ take 3 $ parseLines ss)
