module Advent2022.Day13.Part2Spec (spec) where

import Advent2022.Day13.Base
import Advent2022.Day13.BaseSpec (testLines)
import Advent2022.Day13.Part2
import Data.List (sort)
import Test.Hspec

packets :: [Packet]
packets = parseLines testLines

spec :: Spec
spec = do
  describe "parseLines" $ do
    it "parses non-blank lines into packets" $ do
      parseLines
        [ "[1,1,3,1,1]",
          "[1,1,5,1,1]",
          "",
          "[[1],[2,3,4]]",
          "[[1],4]"
        ]
        `shouldBe` [ PPacket [PInt 1, PInt 1, PInt 3, PInt 1, PInt 1],
                     PPacket [PInt 1, PInt 1, PInt 5, PInt 1, PInt 1],
                     PPacket [PPacket [PInt 1], PPacket [PInt 2, PInt 3, PInt 4]],
                     PPacket [PPacket [PInt 1], PInt 4]
                   ]

  describe "packet sorting" $ do
    it "Sorts packets based on ordering rules" $ do
      sort packets
        `shouldBe` [ PPacket [],
                     PPacket [PPacket []],
                     PPacket [PPacket [PPacket []]],
                     PPacket [PInt 1, PInt 1, PInt 3, PInt 1, PInt 1],
                     PPacket [PInt 1, PInt 1, PInt 5, PInt 1, PInt 1],
                     PPacket [PPacket [PInt 1], PPacket [PInt 2, PInt 3, PInt 4]],
                     PPacket [PInt 1, PPacket [PInt 2, PPacket [PInt 3, PPacket [PInt 4, PPacket [PInt 5, PInt 6, PInt 0]]]], PInt 8, PInt 9],
                     PPacket [PInt 1, PPacket [PInt 2, PPacket [PInt 3, PPacket [PInt 4, PPacket [PInt 5, PInt 6, PInt 7]]]], PInt 8, PInt 9],
                     PPacket [PPacket [PInt 1], PInt 4],
                     PPacket [PInt 3],
                     PPacket [PPacket [PInt 4, PInt 4], PInt 4, PInt 4],
                     PPacket [PPacket [PInt 4, PInt 4], PInt 4, PInt 4, PInt 4],
                     PPacket [PInt 7, PInt 7, PInt 7],
                     PPacket [PInt 7, PInt 7, PInt 7, PInt 7],
                     PPacket [PPacket [PInt 8, PInt 7, PInt 6]],
                     PPacket [PInt 9]
                   ]

  describe "dividerIndices" $ do
    it "finds the 1-based indexes of dividers in a sorted packet list" $ do
      dividerIndices (sort (addDividers packets)) `shouldBe` (10, 14)

  describe "decoderKey" $ do
    it "finds the decoder key of a packet list" $ do
      decoderKey packets `shouldBe` 140
