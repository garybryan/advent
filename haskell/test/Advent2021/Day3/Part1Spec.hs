module Advent2021.Day3.Part1Spec (spec) where

import Advent2021.Day3.Part1
import Test.Hspec

spec :: Spec
spec = do
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
