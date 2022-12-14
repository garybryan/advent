module Advent2022.Day18.Part1Spec (spec) where

import Advent2022.Day18.Part1
import Advent2022.Day18.BaseSpec (points)
import Test.Hspec

spec :: Spec
spec = do
  describe "connected" $ do
    it "is true if points are adjacent" $ do
      connected (1, 1, 1) (2, 1, 1) `shouldBe` True
      connected (1, 3, 1) (1, 4, 1) `shouldBe` True
      connected (1, 3, 7) (1, 3, 6) `shouldBe` True

    it "is false if points are not adjacent" $ do
      connected (1, 1, 1) (2, 2, 1) `shouldBe` False
      connected (1, 3, 1) (4, 4, 4) `shouldBe` False
      connected (1, 1, 1) (2, 2, 2) `shouldBe` False

  describe "nConnected" $ do
    it "gives the number of connections in a list of points" $ do
      nConnected [(1, 1, 1), (2, 2, 1)] `shouldBe` 0
      nConnected [(1, 1, 1), (2, 1, 1)] `shouldBe` 1
      nConnected [(1, 1, 1), (2, 1, 1), (5, 5, 5), (1, 1, 2), (1, 1, 3)] `shouldBe` 3

  describe "surfaceArea" $ do
    it "gives the number of unconnected faces for a list of points" $ do
      surfaceArea [(1, 1, 1), (2, 2, 1)] `shouldBe` 12
      surfaceArea [(1, 1, 1), (2, 1, 1)] `shouldBe` 10
      surfaceArea [(1, 1, 1), (2, 1, 1), (5, 5, 5), (1, 1, 2), (1, 1, 3)] `shouldBe` 24
      surfaceArea points `shouldBe` 64
