module Advent2022.Day13.Part2
  ( parseLines,
    run,
    addDividers,
    dividerIndices,
    decoderKey,
  )
where

import Advent2022.Day13.Base
import Data.List (sort)
import qualified Data.Set as Set

parseLines :: [String] -> [Packet]
parseLines = map parsePacket . filter (/= "")

dividers :: [Int]
dividers = [2, 6]

dividerPacket :: Int -> Packet
dividerPacket x = PPacket [PPacket [PInt x]]

addDividers :: [Packet] -> [Packet]
addDividers = foldr (:) (map dividerPacket dividers)

dividerIndices :: [Packet] -> [Int]
dividerIndices = map fst . filter (isDivider . snd) . zip [1 ..]
  where
    ds = Set.fromList dividers
    isDivider :: Packet -> Bool
    isDivider (PPacket [PPacket [PInt x]])
      | x `Set.member` ds = True
      | otherwise = False
    isDivider _ = False

decoderKey :: [Packet] -> Int
decoderKey = product . dividerIndices . sort . addDividers

decoderKeyFromLines :: [String] -> Int
decoderKeyFromLines = decoderKey . parseLines

run :: [String] -> String
run ss = "Decoder key: " ++ show (decoderKeyFromLines ss)
