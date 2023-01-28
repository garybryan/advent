module Advent2022.Day16.Part1
  ( run,
    maxPressureOneActor,
  )
where

import Advent2022.Day16.Base
import Advent2022.Day16.Parsing

maxPressure :: Int -> Valves -> Int -> Int
maxPressure limit valves startValveIndex =
  fst $ maxPressureAndPressures limit valves True startValveIndex

maxPressureOneActor :: Valves -> Int -> Int
maxPressureOneActor = maxPressure 30

maxPressureFromLines :: [String] -> Int
maxPressureFromLines = uncurry maxPressureOneActor . valveVectorAndStartIndexFromLines

run :: [String] -> String
run ss = "Max pressure from player opening valves: " ++ show (maxPressureFromLines ss)
