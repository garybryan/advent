module Advent2022.Day15.Part1Spec (spec) where

import Advent2022.Day15.BaseSpec (ssAndBs)
import Advent2022.Day15.Part1
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "cannotHaveBeacon" $ do
    let ssAndRs =
          [ ((2, 18), 7),
            ((9, 16), 1)
          ]
    let bs = Set.fromList [(8, 22)]

    it "is false if no sensor radius contains the point" $ do
      cannotHaveBeacon ssAndRs bs (8, 21) `shouldBe` False

    it "is true if a sensor radius contains the point" $ do
      cannotHaveBeacon ssAndRs bs (4, 16) `shouldBe` True

    it "is false if these is a beacon on the point" $ do
      cannotHaveBeacon ssAndRs bs (8, 21) `shouldBe` False

  describe "rowPointsNoBeacon" $ do
    it "gives the points in a row within the bounds that cannot have a beacon" $ do
      rowPointsNoBeacon 9 ssAndBs
        `shouldBe` [ (-1, 9),
                     (0, 9),
                     (1, 9),
                     (2, 9),
                     (3, 9),
                     (4, 9),
                     (5, 9),
                     (6, 9),
                     (7, 9),
                     (8, 9),
                     (9, 9),
                     (10, 9),
                     (11, 9),
                     (12, 9),
                     (13, 9),
                     (14, 9),
                     (15, 9),
                     (16, 9),
                     (17, 9),
                     (18, 9),
                     (19, 9),
                     (20, 9),
                     (21, 9),
                     (22, 9),
                     (23, 9)
                   ]

  describe "nRowPointsNoBeacon" $ do
    it "gives the number of points in a row within the bounds that cannot have a beacon" $ do
      nRowPointsNoBeacon 9 ssAndBs `shouldBe` 25
      nRowPointsNoBeacon 10 ssAndBs `shouldBe` 26
      nRowPointsNoBeacon 11 ssAndBs `shouldBe` 28
