module Advent2022.Day10.Part2Spec (spec) where

import Advent2022.Day10.Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "crtChar" $ do
    it "gives a '.' for a pixel not under the cursor in the first row" $ do
      crtChar 40 (5, 10) `shouldBe` '.'

    it "gives a '.' for a pixel not under the cursor in a subsequent row" $ do
      crtChar 40 (86, 10) `shouldBe` '.'

    it "gives a '#' for a pixel under the cursor in the first row" $ do
      crtChar 40 (5, 3) `shouldBe` '#'
      crtChar 40 (5, 4) `shouldBe` '#'
      crtChar 40 (5, 5) `shouldBe` '#'

    it "gives a '#' for a pixel under the cursor in a subsequent row" $ do
      crtChar 40 (85, 3) `shouldBe` '#'
      crtChar 40 (85, 4) `shouldBe` '#'
      crtChar 40 (85, 5) `shouldBe` '#'
