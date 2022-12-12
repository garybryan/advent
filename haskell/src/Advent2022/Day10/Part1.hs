module Advent2022.Day10.Part1
  ( run,
    signalStrengths,
    signalStrengthFromLines,
  )
where

import Advent2022.Day10.Base

signalStrengthsHelper :: [Int] -> [(Int, Int)] -> [Int]
signalStrengthsHelper [] _ = [] -- Stop execution after the last instruction we care about
signalStrengthsHelper _ [] = []
signalStrengthsHelper cz@(c : cs) ((t, v) : vts)
  | c == t = v * t : signalStrengthsHelper cs vts
  | otherwise = signalStrengthsHelper cz vts

signalStrengths :: [Int] -> [Instr] -> [Int]
signalStrengths cs is = signalStrengthsHelper cs (valuesWithTicks is)

totalSignalStrengths :: [Int] -> [Instr] -> Int
totalSignalStrengths cs is = sum $ signalStrengths cs is

signalStrengthFromLines :: [String] -> Int
signalStrengthFromLines ss = totalSignalStrengths (20 : [60, 100 .. 220]) (map parseLine ss)

run :: [String] -> String
run ls = "Total signal strength: " ++ show (signalStrengthFromLines ls)
