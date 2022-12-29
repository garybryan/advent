module Advent2022.Day16.Part1
  ( run,
    maxPressureOneActor,
  )
where

import Advent2022.Day16.Base

maxPressureOneActor :: ValveMap -> Int -> Int
maxPressureOneActor vm = maxPressure 30 vm (nonZeroValves vm)

maxPressureFromLines :: [String] -> Int
maxPressureFromLines = uncurry maxPressureOneActor . valveMapAndStartIndexFromLines

run :: [String] -> String
run ss = "Max pressure from player opening valves: " ++ show (maxPressureFromLines ss)
