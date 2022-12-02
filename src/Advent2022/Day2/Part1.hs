module Advent2022.Day2.Part1 (run, test) where

import qualified Data.Map as Map

import Lib.Read (readLines)
import Advent2022.Day2.Base hiding (test)

userChoiceMap :: ChoiceParseMap
userChoiceMap = Map.fromList
  [
    ('X', Rock),
    ('Y', Paper),
    ('Z', Scissors)
  ]

charsToRound :: (Char, Char) -> (Choice, Choice)
charsToRound (oc, uc) = 
  (
    parseChar opponentChoiceMap oc,
    parseChar userChoiceMap uc
  )

scoreFromLinesPart1 :: [String] -> Int
scoreFromLinesPart1 = scoreFromLines charsToRound 

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let result = scoreFromLinesPart1 fileLines
  putStrLn ("Part 1 final score: " ++ show result)

test :: IO ()
test = do
  putStrLn ("Parse X: " ++ show (parseChar userChoiceMap 'X') ++ "; should be Rock")

  putStrLn ("Chars to choices (Y, C): " ++ show (charsToRound ('C', 'Y')) ++ "; should be (Scissors,Paper)")

  let gameLines = ["A Y", "B X", "C Z"]
  putStrLn ("Game from lines: " ++ show (scoreFromLinesPart1 gameLines) ++ "; should be 15")

