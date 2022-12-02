module Advent2022.Day2.Part2 (run, test) where

import qualified Data.Map as Map

import Lib.Read (readLines)
import Advent2022.Day2.Base hiding (test)

data Action = Lose | Draw | Win deriving (Eq, Show)

type ActionParseMap = ParseMap Action

actionMap :: ActionParseMap
actionMap = Map.fromList
  [
    ('X', Lose),
    ('Y', Draw),
    ('Z', Win)
  ]

atOffset :: (Enum a, Bounded a) => Int -> a -> a
atOffset n e = toEnum ((ei + n) `mod` modulus)
  where
    modulus = fromEnum (maxBound `asTypeOf` e) + 1
    ei = fromEnum e

choiceForAction :: Action -> Choice -> Choice
choiceForAction Lose = atOffset (-1)
choiceForAction Win  = atOffset 1
choiceForAction Draw = id

charsToRound :: (Char, Char) -> (Choice, Choice)
charsToRound (c, a) = 
  (
    choice,
    choiceForAction (parseChar actionMap a) choice
  )
  where choice = parseChar opponentChoiceMap c

scoreFromLinesPart2 :: [String] -> Int
scoreFromLinesPart2 = scoreFromLines charsToRound 

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let result = scoreFromLinesPart2 fileLines
  putStrLn ("Part 2 final score: " ++ show result)

test :: IO ()
test = do
  putStrLn ("Parse X: " ++ show (parseChar actionMap 'X') ++ "; should be Lose")

  putStrLn ("To win against Scissors: " ++ show (choiceForAction Win Scissors) ++ "; should be Rock")
  putStrLn ("To lose against Rock: " ++ show (choiceForAction Lose Rock) ++ "; should be Scissors")
  putStrLn ("To draw with Paper: " ++ show (choiceForAction Draw Paper) ++ "; should be Paper")

  putStrLn ("Chars to round (C, Z): " ++ show (charsToRound ('C', 'Z')) ++ "; should be (Scissors,Rock)")

  let gameLines = ["A Y", "B X", "C Z"]
  putStrLn ("Game from lines: " ++ show (scoreFromLinesPart2 gameLines) ++ "; should be 12")

