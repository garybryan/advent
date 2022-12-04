module Advent2022.Day2.Part1 (run, scoreFromLinesPart1) where

import Advent2022.Day2.Base
import qualified Data.Map as Map

userChoiceMap :: ChoiceParseMap
userChoiceMap =
  Map.fromList
    [ ('X', Rock),
      ('Y', Paper),
      ('Z', Scissors)
    ]

charsToRound :: (Char, Char) -> Round
charsToRound (oc, uc) =
  ( parseChar opponentChoiceMap oc,
    parseChar userChoiceMap uc
  )

scoreFromLinesPart1 :: [String] -> Int
scoreFromLinesPart1 = scoreFromLines charsToRound

run :: [String] -> String
run ls = "Final score: " ++ show (scoreFromLinesPart1 ls)
