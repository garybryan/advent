module Advent2022.Day02.BaseSpec (spec) where

import Advent2022.Day02.Base
import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  describe "gameScore" $ do
    it "calculates the total game score" $ do
      let game = [(Paper, Rock), (Rock, Paper), (Scissors, Scissors)]
      gameScore game `shouldBe` 15

  describe "scoreFromLines" $ do
    it "calculates the total score with Rock vs Paper every round" $ do
      scoreFromLines (const (Rock, Paper)) ["A Y", "B X", "C Z"] `shouldBe` 24

  describe "parseChar" $ do
    it "parses a character for an opponent choice" $ do
      parseChar opponentChoiceMap 'B' `shouldBe` Paper

    it "throws an exception if trying to parse an invalid character" $ do
      evaluate (parseChar opponentChoiceMap 'E') `shouldThrow` errorCall "Invalid char: 'E'"
