module Advent2022.Day13.Part1Spec (spec) where

import Advent2022.Day13.Base
import Advent2022.Day13.BaseSpec (testLines)
import Advent2022.Day13.Part1
import Test.Hspec

pairs :: [Pair]
pairs = parseLines testLines

spec :: Spec
spec = do
  describe "parseLines" $ do
    it "parses lines into pairs of packets" $ do
      parseLines
        [ "[1,1,3,1,1]",
          "[1,1,5,1,1]",
          "",
          "[[1],[2,3,4]]",
          "[[1],4]"
        ]
        `shouldBe` [ ( PPacket [PInt 1, PInt 1, PInt 3, PInt 1, PInt 1],
                       PPacket [PInt 1, PInt 1, PInt 5, PInt 1, PInt 1]
                     ),
                     ( PPacket [PPacket [PInt 1], PPacket [PInt 2, PInt 3, PInt 4]],
                       PPacket [PPacket [PInt 1], PInt 4]
                     )
                   ]

  describe "indicesInOrder" $ do
    it "gives the 1-based indices of pairs that are in order" $ do
      indicesInOrder pairs `shouldBe` [1, 2, 4, 6]

  describe "indicesSum" $ do
    it "gives the sum of the 1-based indices of pairs that are in order" $ do
      indexSum pairs `shouldBe` 13
