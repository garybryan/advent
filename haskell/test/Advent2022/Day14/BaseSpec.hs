module Advent2022.Day14.BaseSpec (spec) where

import Advent2022.Day14.Base
import qualified Data.Set as Set
import Test.Hspec

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses a text line to a list of points" $ do
      parseLine "498,4 -> 498,6 -> 496,6" `shouldBe` [(498, 4), (498, 6), (496, 6)]

  describe "pointsOnLine" $ do
    it "gets all points on a horizontal line" $ do
      pointsOnLine (498, 4) (498, 7) `shouldBe` [(498, 4), (498, 5), (498, 6), (498, 7)]

    it "gets all points on a vertical line" $ do
      pointsOnLine (9, 4) (5, 4) `shouldBe` [(5, 4), (6, 4), (7, 4), (8, 4), (9, 4)]

  describe "pointsOnLines" $ do
    it "gets all points on the lines from a list of points" $ do
      pointsOnLines [(498, 4), (498, 6), (496, 6)]
        `shouldBe` Set.fromList [(498, 4), (498, 5), (498, 6), (497, 6), (496, 6)]

  describe "pointsOnLines" $ do
    it "gets all points on the lines from lists of points" $ do
      pointsOnAllLines
        [ [(498, 4), (498, 6), (496, 6)],
          [(496, 6), (494, 6)]
        ]
        `shouldBe` Set.fromList
          [(498, 4), (498, 5), (498, 6), (497, 6), (496, 6), (495, 6), (494, 6)]

  describe "maxYOfPoints" $ do
    it "gives the highest Y coordinate of a set of points" $ do
      maxYOfPoints (Set.fromList [(4, 4), (5, 4), (4, 5), (5, 5), (6, 5)]) `shouldBe` 5
