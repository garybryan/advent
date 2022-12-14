module Advent2022.Day12.BaseSpec (spec, ls, hills, end) where

import Advent2022.Day12.Base
import Control.Exception (evaluate)
import qualified Data.Dequeue as DQ
import qualified Data.Matrix as M
import qualified Data.Set as Set
import Test.Hspec

ls =
  [ "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi"
  ]

deadEndLs = ["SdE"]

(hills, start, end) = parseLines ls

(deHills, deStart, deEnd) = parseLines deadEndLs

spec :: Spec
spec = do
  describe "parseLines" $ do
    it "parses lines into a matrix with start and end points." $ do
      parseLines
        [ "Sab",
          "ceb",
          "xEz"
        ]
        `shouldBe` ( M.fromLists
                       [ [0, 0, 1],
                         [2, 4, 1],
                         [23, 25, 25]
                       ],
                     (1, 1),
                     (3, 2)
                   )

  describe "accessibleNeighbours" $ do
    it "finds the neighbours of a point that are at most one step higher" $ do
      Set.fromList (accessibleNeighbours (1, 1) hills) `shouldBe` Set.fromList [(1, 2), (2, 1)]

    it "finds the neighbours of a point that are at the same level" $ do
      Set.fromList (accessibleNeighbours (3, 3) hills) `shouldBe` Set.fromList [(3, 2), (2, 3), (4, 3)]

    it "finds the neighbours of a point that are lower or one step higher" $ do
      Set.fromList (accessibleNeighbours (4, 4) hills) `shouldBe` Set.fromList [(4, 3), (3, 4), (4, 5), (5, 4)]

  describe "bfs" $ do
    it "finds the shortest path to a target point" $ do
      bfs end (DQ.fromList [start]) hills (initialDistances hills) `shouldBe` Just 31

    it "finds no path if there is no possible path" $ do
      bfs deEnd (DQ.fromList [deStart]) deHills (initialDistances hills) `shouldBe` Nothing

  describe "bfsFrom" $ do
    it "finds the shortest path from one point to another" $ do
      bfsFrom start end hills `shouldBe` 31

    it "gives an error if there is no path" $ do
      evaluate (bfsFrom deStart deEnd deHills) `shouldThrow` errorCall "No path found"
