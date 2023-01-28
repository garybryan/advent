module Advent2022.Day16.Part1Spec (spec) where

import Advent2022.Day16.BaseSpec (valves)
import Advent2022.Day16.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "maxPressureOneActor" $ do
    it "finds the maximum possible pressure for 30 turns and one actor" $ do
      maxPressureOneActor valves 0 `shouldBe` 1651
