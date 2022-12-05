module Advent2022.Day05.Part2Spec (spec) where

import Advent2022.Day05.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "doThing" $ do
    it "does a thing" $ do
      doThing `shouldBe` "Did a thing, part 2."
