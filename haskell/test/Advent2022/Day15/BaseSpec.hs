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

  describe "rowSensorRange" $ do
    -- The sensor at (13, 2)
    let sAndR = sensorsAndRadii ssAndBs !! 2

    it "gives the range covered by a sensor in a single row" $ do
      rowSensorRange 1 sAndR `shouldBe` Just (11, 15)
      rowSensorRange 2 sAndR `shouldBe` Just (10, 16)
      rowSensorRange 5 sAndR `shouldBe` Just (13, 13)

    it "gives Nothing if the sensor does not cover the row" $ do
      rowSensorRange 6 sAndR `shouldBe` Nothing
