module Advent2022.Day19.Part2Spec (spec) where

import Advent2022.Day19.BaseSpec (blueprints)
import Advent2022.Day19.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "products" $ do
    it "multiplies the numbers of geodes from the blueprints" $ do
      products (take 1 blueprints) `shouldBe` 56
      products (take 3 blueprints) `shouldBe` 3472
