module Advent2022.Day14.Part1Spec (spec) where

import Advent2022.Day14.Part1
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "sandFall" $ do
    it "gives the number of units of sand that can come to rest before others fall into the void" $ do
      let ps = Set.fromList [(3, 5), (4, 5), (5, 5), (6, 5)]
      sandFall 5 (5, 0) ps `shouldBe` 2

  describe "sandFallFromLines" $ do
    it "gives the number of units of sand that come to rest given text lines defining points" $ do
      let ls =
            [ "498,4 -> 498,6 -> 496,6",
              "503,4 -> 502,4 -> 502,9 -> 494,9"
            ]
      sandFallFromLines ls `shouldBe` 24
