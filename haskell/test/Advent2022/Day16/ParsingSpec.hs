module Advent2022.Day16.ParsingSpec (spec) where

import Advent2022.Day16.Base (Valve (..))
import Advent2022.Day16.Parsing
import qualified Data.Vector as V
import Test.Hspec

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses a line to a valve name, rate, and adjacent names" $ do
      parseLine "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
        `shouldBe` ("AA", (0, ["DD", "II", "BB"]))

    it "parses a line to a valve name, rate, and single adjacent valve" $ do
      parseLine "Valve AA has flow rate=0; tunnel leads to valve DD"
        `shouldBe` ("AA", (0, ["DD"]))

  describe "valveVectorAndStartIndexFromLines" $ do
    it "parses line to a valve map and start index" $ do
      valveVectorAndStartIndexFromLines
        [ "Valve BB has flow rate=13; tunnels lead to valves AA, CC",
          "Valve AA has flow rate=0; tunnels lead to valves BB, CC",
          "Valve CC has flow rate=5; tunnels lead to valves AA, BB"
        ]
        `shouldBe` ( V.fromList
                       [ Valve 13 [1, 2],
                         Valve 0 [0, 2],
                         Valve 5 [1, 0]
                       ],
                     1
                   )
