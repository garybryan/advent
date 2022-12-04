module Advent_Y_.Day_D_.Part1Spec (spec) where

import Advent_Y_.Day_D_.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "doThing" $ do
    it "does a thing" $ do
      doThing `shouldBe` "Did a thing."
