module Advent2021.Day3.BaseSpec (spec) where

import Advent2021.Day3.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "binStrToInt" $ do
    it "converts a string of binary digits representing an unsigned integer to said integer" $ do
      binStrToInt "0101" `shouldBe` 5
      binStrToInt "10000000" `shouldBe` 128

  describe "bitFreqs" $ do
    it "Gets the bit frequencies for a list of integers" $ do
      bitFreqs 5 [5] `shouldBe` [0, 0, 1, 0, 1]
      bitFreqs 5 [9, 3, 1] `shouldBe` [0, 1, 0, 1, 3]

