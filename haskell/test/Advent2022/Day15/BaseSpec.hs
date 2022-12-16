module Advent2022.Day15.BaseSpec (spec, ssAndBs) where

import Advent2022.Day15.Base
import Lib.Types (Point)
import Test.Hspec

ssAndBs :: [(Point, Point)]
ssAndBs =
  [ ((2, 18), (-2, 15)),
    ((9, 16), (10, 16)),
    ((13, 2), (15, 3)),
    ((12, 14), (10, 16)),
    ((10, 20), (10, 16)),
    ((14, 17), (10, 16)),
    ((8, 7), (2, 10)),
    ((2, 0), (2, 10)),
    ((0, 11), (2, 10)),
    ((20, 14), (25, 17)),
    ((17, 20), (21, 22)),
    ((16, 7), (15, 3)),
    ((14, 3), (15, 3)),
    ((20, 1), (15, 3))
  ]

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses the sensor and beacon points from a line" $ do
      parseLine "Sensor at x=2, y=18: closest beacon is at x=-2, y=15" `shouldBe` ((2, 18), (-2, 15))

  describe "sensorsAndRadii" $ do
    it "gets the sensor points and their radii based on closest beacon" $ do
      sensorsAndRadii [((2, 18), (-2, 15)), ((9, 16), (10, 16))]
        `shouldBe` [ ((2, 18), 7),
                     ((9, 16), 1)
                   ]

  describe "xBounds" $ do
    it "gets the lower and upper x-axis bounds for the area containing all sensors' ranges" $ do
      xBounds (sensorsAndRadii ssAndBs) `shouldBe` (-8, 28)
