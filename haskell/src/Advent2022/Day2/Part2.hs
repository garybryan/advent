module Advent2022.Day2.Part2 (run, scoreFromLinesPart2) where

import Advent2022.Day2.Base
import qualified Data.Map as Map

data Action = Lose | Draw | Win deriving (Eq, Show)

type ActionParseMap = ParseMap Action

actionMap :: ActionParseMap
actionMap =
  Map.fromList
    [ ('X', Lose),
      ('Y', Draw),
      ('Z', Win)
    ]

atOffset :: (Enum a, Bounded a) => Int -> a -> a
atOffset o e = toEnum ((ei + o) `mod` modulus)
  where
    modulus = fromEnum (maxBound `asTypeOf` e) + 1
    ei = fromEnum e

choiceForAction :: Action -> Choice -> Choice
choiceForAction Lose = atOffset (-1)
choiceForAction Win = atOffset 1
choiceForAction Draw = id

charsToRound :: (Char, Char) -> Round
charsToRound (c, a) =
  ( choice,
    choiceForAction (parseChar actionMap a) choice
  )
  where
    choice = parseChar opponentChoiceMap c

scoreFromLinesPart2 :: [String] -> Int
scoreFromLinesPart2 = scoreFromLines charsToRound

run :: [String] -> String
run ls = "Final score: " ++ show (scoreFromLinesPart2 ls)
