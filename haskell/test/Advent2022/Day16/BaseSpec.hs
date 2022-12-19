module Advent2022.Day16.BaseSpec (spec) where

import Advent2022.Day16.Base
import qualified Data.Map as Map
import Test.Hspec

valveMap :: ValveMap
valveMap =
  Map.fromList
    [ (1, Valve 0 [4, 9, 2]),
      (2, Valve 13 [3, 1]),
      (3, Valve 2 [4, 2]),
      (4, Valve 20 [3, 1, 5]),
      (5, Valve 3 [6, 4]),
      (6, Valve 0 [5, 7]),
      (7, Valve 0 [6, 8]),
      (8, Valve 22 [7]),
      (9, Valve 0 [1, 10]),
      (10, Valve 21 [9])
    ]

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses a line to a valve name, rate, and adjacent names" $ do
      parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
        `shouldBe` ("AA", (0, ["DD", "II", "BB"]))

    it "parses a line to a valve name, rate, and single adjacent valve" $ do
      parseLine "Valve AA has flow rate=0; tunnel leads to valve DD"
        `shouldBe` ("AA", (0, ["DD"]))

  describe "valveMapAndStartIndexFromLines" $ do
    it "parses line to a valve map and start index" $ do
      valveMapAndStartIndexFromLines
        [ "Valve BB has flow rate=13; tunnels lead to valves AA, CC",
          "Valve AA has flow rate=0; tunnels lead to valves BB, CC",
          "Valve CC has flow rate=5; tunnels lead to valves AA, BB"
        ]
        `shouldBe` ( Map.fromList
                       [ (1, Valve 13 [2, 3]),
                         (2, Valve 0 [1, 3]),
                         (3, Valve 5 [2, 1])
                       ],
                     2
                   )

  describe "maxPressure" $ do
    it "finds the maximum possible pressure" $ do
      maxPressure 30 valveMap 1 `shouldBe` 1651
