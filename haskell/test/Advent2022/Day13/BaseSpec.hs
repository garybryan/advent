module Advent2022.Day13.BaseSpec (spec, testLines) where

import Advent2022.Day13.Base
import Test.Hspec

testLines :: [String]
testLines =
  [ "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]

spec :: Spec
spec = do
  describe "parsePacket" $ do
    it "parses a packet containing numbers and lists" $ do
      parsePacket "[[1],5,[2,3,[54]]]" `shouldBe` PPacket [PPacket [PInt 1], PInt 5, PPacket [PInt 2, PInt 3, PPacket [PInt 54]]]

  describe "isInOrder" $ do
    describe "compare two integers" $ do
      it "is true if the left integer is lower" $ do
        isInOrder (PInt 4, PInt 8) `shouldBe` Just True

      it "is false if the left integer is higher" $ do
        isInOrder (PInt 77, PInt 6) `shouldBe` Just False

      it "indicates if the integers are equal" $ do
        isInOrder (PInt 6, PInt 6) `shouldBe` Nothing

    describe "compare integers to lists" $ do
      it "is true if the left integer is lower" $ do
        isInOrder (PPacket [PInt 4], PInt 8) `shouldBe` Just True

      it "is false if the left integer is higher" $ do
        isInOrder (PInt 77, PPacket [PInt 6]) `shouldBe` Just False

    describe "compare lists of integers" $ do
      it "is true if the first left integer is lower than the first right" $ do
        isInOrder (PPacket [PInt 4, PInt 20], PPacket [PInt 8, PInt 2]) `shouldBe` Just True

      it "is false if the first left integer is higher than the first right" $ do
        isInOrder (PPacket [PInt 77, PInt 2], PPacket [PInt 6, PInt 5]) `shouldBe` Just False

      it "is true if the left list runs out of items first" $ do
        isInOrder (PPacket [PInt 4, PInt 10], PPacket [PInt 4, PInt 10, PInt 20]) `shouldBe` Just True

      it "is false if the right list runs out of items first" $ do
        isInOrder (PPacket [PInt 4, PInt 10, PInt 20], PPacket [PInt 4, PInt 10]) `shouldBe` Just False
        isInOrder (PPacket [PInt 7, PInt 7, PInt 7], PPacket [PInt 7, PInt 7]) `shouldBe` Just False

      it "is equal if both lists run out of items" $ do
        isInOrder (PPacket [PInt 4, PInt 10], PPacket [PInt 4, PInt 10]) `shouldBe` Nothing

  describe "compare mixed lists" $ do
    it "compares an integer to a list" $ do
      isInOrder (PPacket [PInt 9], PPacket [PPacket [PInt 8, PInt 7]]) `shouldBe` Just False

    it "compares a list of integers to a list of lists of integers" $ do
      isInOrder (PPacket [PInt 4, PInt 10], PPacket [PPacket [PInt 4, PInt 20], PInt 5]) `shouldBe` Just True

    it "compares a list of integers to a list of a list then integers" $ do
      isInOrder (PPacket [PInt 4, PInt 10], PPacket [PPacket [PInt 4], PInt 20, PInt 5]) `shouldBe` Just True
