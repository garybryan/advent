module Advent2022.Day18.BaseSpec (points, spec) where

import Advent2022.Day18.Base
import Test.Hspec

points :: [Point]
points =
  [ (2, 2, 2),
    (1, 2, 2),
    (3, 2, 2),
    (2, 1, 2),
    (2, 3, 2),
    (2, 2, 1),
    (2, 2, 3),
    (2, 2, 4),
    (2, 2, 6),
    (1, 2, 5),
    (3, 2, 5),
    (2, 1, 5),
    (2, 3, 5)
  ]

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses a line to a 3D point" $ do
      parseLine "2,3,5" `shouldBe` (2, 3, 5)
