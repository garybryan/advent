module Advent2022.Day15.Part1Spec (spec) where

import Advent2022.Day15.Base
import Advent2022.Day15.BaseSpec (ssAndBs)
import Advent2022.Day15.Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "rowSensorRanges" $ do
    it "gives the ranges in a row covered by any sensor" $ do
      rowSensorRanges 11 (sensorsAndRadii ssAndBs)
        `shouldBe` [(2, 2), (11, 13), (3, 13), (-3, 3), (15, 25), (15, 17)]

  describe "rangeTotal" $ do
    it "gives the total length of non-overlapping ranges" $ do
      rangeTotal [(1, 3), (13, 16), (5, 11)] `shouldBe` 14

    it "gives the total length of ranges with overlaps" $ do
      rangeTotal [(1, 5), (13, 16), (4, 7)] `shouldBe` 11

  describe "nRowPointsNoBeacon" $ do
    it "gives the number of points in a row within the bounds that cannot have a beacon" $ do
      nRowPointsNoBeacon 9 ssAndBs `shouldBe` 25
      nRowPointsNoBeacon 10 ssAndBs `shouldBe` 26
      nRowPointsNoBeacon 11 ssAndBs `shouldBe` 28
