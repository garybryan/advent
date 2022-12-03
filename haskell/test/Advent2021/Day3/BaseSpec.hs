module Advent2021.Day3.BaseSpec (spec) where

import Advent2021.Day3.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "binStrToInt" $ do
    it "converts a string of binary digits representing an unsigned integer to said integer" $ do
      binStrToInt "0101" `shouldBe` 5
      binStrToInt "10000000" `shouldBe` 128

  describe "bit frequencies" $ do
    it "Gets the bit frequencies for a list of integers" $ do
      bitFreqs 5 [5] `shouldBe` [0, 0, 1, 0, 1]
      bitFreqs 5 [9, 3, 1] `shouldBe` [0, 1, 0, 1, 3]

  describe "most frequent" $ do
    it "Gets the most frequent bit values for a list of integers" $ do
      mostFrequent 5 [5] `shouldBe` [0, 0, 1, 0, 1]
      mostFrequent 5 [10, 8, 9, 3, 1] `shouldBe` [0, 1, 0, 0, 1]

  describe "gamma rate" $ do
    it "Gets the gamma rate for a list of integers" $ do
      gammaRate 5 [4, 30, 22, 23, 21, 15, 7, 28, 16, 25, 2, 10] `shouldBe` 22

  describe "epsilon rate" $ do
    it "Gets the epsilon rate for a gamma rate" $ do
      epsilonRate 5 22 `shouldBe` 9
      epsilonRate 8 0 `shouldBe` 255

  describe "power consumption" $ do
    it "Gets the power consumption for a list of integers" $ do
      powerConsumption 5 [4, 30, 22, 23, 21, 15, 7, 28, 16, 25, 2, 10] `shouldBe` 198

  describe "power consumption from lines" $ do
    it "Gets the power consumption for a list of binary strings" $ do
      powerConsumptionFromLines ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"] `shouldBe` 198
