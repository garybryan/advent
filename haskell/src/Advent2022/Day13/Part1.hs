module Advent2022.Day13.Part1
  ( run,
    parseLines,
    indicesInOrder,
    indexSum,
    indexSumFromLines,
  )
where

import Advent2022.Day13.Base
import Data.List.Split (splitWhen)

parsePair :: [String] -> Pair
parsePair ss = (parsePacket $ head ss, parsePacket $ last ss)

parseLines :: [String] -> [Pair]
parseLines = map parsePair . splitWhen (== "")

indicesInOrder :: [Pair] -> [Int]
indicesInOrder = map fst . filter (uncurry (<) . snd) . zip [1 ..]

indexSum :: [Pair] -> Int
indexSum = sum . indicesInOrder

indexSumFromLines :: [String] -> Int
indexSumFromLines = indexSum . parseLines

run :: [String] -> String
run ls = "Sum of indicies of pairs in right order: " ++ show (indexSumFromLines ls)
