module Advent2022.Day13.Part2
  ( parseLines,
    run,
    addDividers,
    dividerIndices,
    decoderKey,
  )
where

import Advent2022.Day13.Base
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)

parseLines :: [String] -> [Packet]
parseLines = map parsePacket . filter (/= "")

divider :: Int -> Packet
divider x = PPacket [PPacket [PInt x]]

addDividers :: [Packet] -> [Packet]
addDividers = (:) (divider 2) . (:) (divider 6)

dividerIndices :: [Packet] -> (Int, Int)
dividerIndices ps = (packetIndex 2, packetIndex 6)
  where
    packetIndex :: Int -> Int
    packetIndex x = fromMaybe (error $ "Divider packet not found" ++ show x) (elemIndex (divider x) ps) + 1

decoderKey :: [Packet] -> Int
decoderKey = uncurry (*) . dividerIndices . sort . addDividers

decoderKeyFromLines :: [String] -> Int
decoderKeyFromLines = decoderKey . parseLines

run :: [String] -> String
run ss = "Decoder key: " ++ show (decoderKeyFromLines ss)
