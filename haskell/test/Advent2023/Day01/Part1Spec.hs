module Advent2023.Day01.Part1Spec (spec) where

import Advent2023.Day01.Base
import Advent2023.Day01.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "calibrationValue with digits" $ do
    it "adds first and last digits of a string" $ do
      calibrationValue digits "pqr3stu8vwx" `shouldBe` 38
      calibrationValue digits "a1b2c3d4e5f" `shouldBe` 15
      calibrationValue digits "treb7uchet" `shouldBe` 77
