module Advent2022.Day11.Part1
  ( run,
  )
where

import Advent2022.Day11.Base

levelAfterRounds :: [String] -> Int
levelAfterRounds = monkeyBusinessLevel 2 . doRounds 20 3 . linesToMonkeys

run :: [String] -> String
run ls = "Monkey business level after 20 rounds: " ++ show (levelAfterRounds ls)
