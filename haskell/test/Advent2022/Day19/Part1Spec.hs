module Advent2022.Day19.Part1Spec (spec) where

import Advent2022.Day19.BaseSpec (blueprints)
import Advent2022.Day19.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "qualityLevel" $ do
    it "gives the quality level of a blueprint" $ do
      qualityLevel (blueprints !! 1) `shouldBe` 24
      qualityLevel (head blueprints) `shouldBe` 9

    describe "qualityLevels" $ do
      it "gives the total of quality levels for all blueprints" $ do
        qualityLevels blueprints `shouldBe` 33
