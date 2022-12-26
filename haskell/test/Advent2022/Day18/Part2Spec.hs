module Advent2022.Day18.Part2Spec (spec) where

import Advent2022.Day18.BaseSpec (points)
import Advent2022.Day18.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "boundingBox" $ do
    it "gives the bounding box diagonal extremes for points on each axis" $ do
      boundingBox points `shouldBe` ((0, 0, 0), (4, 4, 7))

  describe "externalSurfaceArea" $ do
    it "gives the external surface area of the cubes" $ do
      externalSurfaceArea [(0, 0, 0)] `shouldBe` 6
      externalSurfaceArea points `shouldBe` 58
