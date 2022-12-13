module Advent2022.Day11.Part2
  ( run,
  )
where

import Advent2022.Day11.Base

levelAfterRounds :: [String] -> Int
levelAfterRounds = monkeyBusinessLevel 2 . doRounds 10000 1 . linesToMonkeys

run :: [String] -> String
run ls = "Monkey business level after 10000 rounds: " ++ show (levelAfterRounds ls)
