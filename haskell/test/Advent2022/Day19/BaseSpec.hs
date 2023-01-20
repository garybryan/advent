module Advent2022.Day19.BaseSpec (spec, blueprints, v) where

import Advent2022.Day19.Base
import qualified Data.Vector as V
import Test.Hspec

-- Save some typing
v :: [a] -> V.Vector a
v = V.fromList

blueprints :: [Blueprint]
blueprints =
  [ ( 1,
      v
        [ v [4, 0, 0, 0],
          v [2, 0, 0, 0],
          v [3, 14, 0, 0],
          v [2, 0, 7, 0]
        ]
    ),
    ( 2,
      v
        [ v [2, 0, 0, 0],
          v [3, 0, 0, 0],
          v [3, 8, 0, 0],
          v [3, 0, 12, 0]
        ]
    )
  ]

spec :: Spec
spec = do
  let costs = snd $ blueprints !! 1

  describe "getMaxCosts" $ do
    it "gets the max costs for each resource type for any robot" $ do
      getMaxCosts costs `shouldBe` v [3, 8, 12]

  describe "canBuild" $ do
    let cost = v [3, 14, 0, 0]

    it "is true if there are all the necessary robots to build the robot" $ do
      canBuild cost (v [2, 1, 3, 0]) `shouldBe` True

    it "is false if there are not the necessary robots to build the robot" $ do
      canBuild cost (v [0, 3, 0, 1]) `shouldBe` False

  describe "shouldBuild" $ do
    let maxCosts = v [3, 8, 12]

    it "is true if there are fewer robots than the max cost of a resource" $ do
      shouldBuild maxCosts 1 (v [3, 6, 1]) `shouldBe` True

    it "is false if there are enough robots to meet max cost of a resource" $ do
      shouldBuild maxCosts 0 (v [3, 6, 1]) `shouldBe` False

    it "should always build more geode robots!" $ do
      shouldBuild maxCosts 3 (v [3, 6, 1]) `shouldBe` True

  describe "buildRobot" $ do
    it "buils a robot in one minute when resources are available, and updates state" $ do
      let state =
            State
              (v [1, 2, 3, 4])
              (v [4, 14, 5, 0])
              5
      buildRobot costs 2 state
        `shouldBe` State
          (v [1, 2, 4, 4])
          (v [2, 8, 8, 4])
          6

    it "buils a robot in several minutes when resources aren't available but can be produced, and updates state" $ do
      let state =
            State
              (v [1, 2, 3, 4])
              (v [4, 14, 5, 0])
              5

      -- Geode robot costs 12 obsidian; currently have 3 obsidian robots and 5
      -- obsidian, so need 3 turns to have enough plus 1 to build.
      -- 12 obsidian needed, currently have 5, so need 7 more (cost - cur resource).
      -- Producing 3 per turn so need 3 turns to make 7: ceil (7 / 3)
      buildRobot costs 3 state
        `shouldBe` State
          (v [1, 2, 3, 5])
          (v [5, 22, 5, 16])
          9

  describe "bnbFromInitial" $ do
    it "gets the best number of geodes using branch and bound" $ do
      -- bnbFromInitial 24 costs `shouldBe` 12
      bnbFromInitial 24 (snd $ head blueprints) `shouldBe` 9
