module Advent2022.Day16.BaseSpec (spec, valves) where

import Advent2022.Day16.Base
import qualified Data.Matrix as M
import qualified Data.Vector as V
import Test.Hspec

valves :: Valves
valves =
  V.fromList
    [ Valve 0 [3, 8, 1],
      Valve 13 [2, 0],
      Valve 2 [3, 1],
      Valve 20 [2, 0, 4],
      Valve 3 [5, 3],
      Valve 0 [4, 6],
      Valve 0 [5, 7],
      Valve 22 [6],
      Valve 0 [0, 9],
      Valve 21 [8]
    ]

fewerValves :: Valves
fewerValves =
  V.fromList
    [ Valve 0 [1, 2],
      Valve 5 [0],
      Valve 3 [0]
    ]

spec :: Spec
spec = do
  describe "initialMatrix" $ do
    it "makes an adjacency matrix with 1 for adjacent, -1 for not" $ do
      initialDistances fewerValves
        `shouldBe` M.fromLists
          [ [0, 1, 1],
            [1, 0, -1],
            [1, -1, 0]
          ]

  describe "shortestPaths" $ do
    it "makes a matrix with the shortest paths between each pair of valves" $ do
      shortestPaths fewerValves
        `shouldBe` M.fromLists
          [ [0, 1, 1],
            [1, 0, 2],
            [1, 2, 0]
          ]
