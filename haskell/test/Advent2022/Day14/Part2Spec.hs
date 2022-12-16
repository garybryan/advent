module Advent2022.Day14.Part2Spec (spec) where

import Advent2022.Day14.Part2
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "sandFallFromLines" $ do
    it "gives the number of units of sand that come to rest given text lines defining points" $ do
      let ls =
            [ "498,4 -> 498,6 -> 496,6",
              "503,4 -> 502,4 -> 502,9 -> 494,9"
            ]
      sandFallFromLines ls `shouldBe` 93
