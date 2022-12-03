module Advent2021.Day3.BaseSpec (spec) where

import Advent2021.Day3.Base
import Test.Hspec

spec :: Spec
spec = do
  describe "binStrToInt" $ do
    it "converts a string of binary digits representing a positive integer to said integer" $ do
      binStrToInt "0101" `shouldBe` 5
      binStrToInt "10000000" `shouldBe` 128
