module Advent2022.Day2.Base
  (
    Choice (..),
    ChoiceParseMap,
    ParseMap,
    gameScore,
    lineToChars,
    opponentChoiceMap,
    parseChar,
    scoreFromLines,
    test
  )
  where

import qualified Data.Map as Map

-- Define `N` choices as an enumeration with a circular ordering,
-- such that choice `k` is always beaten by choice `k + 1 mod N`.
-- This allows us to use comparison to check which choice wins,
-- and easily get the choice required to win or lose against another choice
-- by getting the successor or predecessor respectively, again `mod N`.
-- The choices are also ordered such that the score for choice `k` is `k + 1`.
data Choice = Rock | Paper | Scissors deriving (Bounded, Eq, Ord, Enum, Show)

choiceScore :: Choice -> Int
choiceScore = (+1) . fromEnum

-- TODO could use instance instead of this? It got stuck when I tried though.
compareCirc :: (Bounded a, Ord a) => a -> a -> Ordering
compareCirc c1 c2
  | c1 == minBound && c2 == maxBound = GT
  | c1 == maxBound && c2 == minBound = LT
  | otherwise                        = compare c1 c2

resultScore :: Choice -> Choice -> Int
resultScore oc uc = case compareCirc uc oc of
  GT -> 6
  EQ -> 3
  LT -> 0

roundScore :: Choice -> Choice -> Int
roundScore oc uc = resultScore oc uc + choiceScore uc

gameScore :: [(Choice, Choice)] -> Int
gameScore = sum . map (\(oc, uc) -> roundScore oc uc)

type ParseMap a = Map.Map Char a
type ChoiceParseMap = ParseMap Choice

opponentChoiceMap :: ChoiceParseMap
opponentChoiceMap = Map.fromList
  [
    ('A', Rock),
    ('B', Paper),
    ('C', Scissors)
  ]

parseChar :: ParseMap a -> Char -> a
parseChar cpm c = case Map.lookup c cpm of
  Nothing     -> error ("Invalid choice char: " ++ show c)
  Just val    -> val

lineToChars :: String -> (Char, Char)
lineToChars line =
  (
    head (head splitLine),
    head (last splitLine)
  )
  where splitLine = words line

scoreFromLines :: ((Char, Char) -> (Choice, Choice)) -> [String] -> Int
scoreFromLines charsFn = gameScore . map (charsFn . lineToChars)

test :: IO ()
test = do
  -- TODO: learn use a proper unit test system for Haskell! This is very DIY.

  putStrLn ("Rock score: " ++ show (choiceScore Rock) ++ "; should be 1")
  putStrLn ("Scissors score: " ++ show (choiceScore Scissors) ++ "; should be 3")

  putStrLn ("Compare Rock to Scissors: " ++ show (compareCirc Rock Scissors) ++ "; should be GT")
  putStrLn ("Compare Rock to Paper: " ++ show (compareCirc Rock Paper) ++ "; should be LT")
  putStrLn ("Compare Rock to Rock: " ++ show (compareCirc Rock Rock) ++ "; should be EQ")

  putStrLn ("Rock vs scissors result score: " ++ show (resultScore Rock Scissors) ++ "; should be 6")
  putStrLn ("Paper vs paper result score: " ++ show (resultScore Paper Paper) ++ "; should be 3")
  putStrLn ("Paper vs scissors result score: " ++ show (resultScore Paper Scissors) ++ "; should be 0")

  putStrLn ("Paper vs rock round score: " ++ show (roundScore Paper Rock) ++ "; should be 8")
  putStrLn ("Rock vs paper round score: " ++ show (roundScore Rock Paper) ++ "; should be 1")
  putStrLn ("Scissors vs scissors round score: " ++ show (roundScore Scissors Scissors) ++ "; should be 6")

  let game = [(Paper, Rock), (Rock, Paper), (Scissors, Scissors)]
  putStrLn ("Game score: " ++ show (gameScore game) ++ "; should be 15")

  putStrLn ("Parse B: " ++ show (parseChar opponentChoiceMap 'B') ++ "; should be Paper")

  putStrLn ("Line to chars (C, Y): " ++ show (lineToChars "C Y") ++ "; should be ('C','Y')")

  -- TODO test for error
  -- putStrLn ("Parse unknown: " ++ show (parseChar opponentChoiceMap 'E') ++ "; should raise error")

  let gameLines = ["A Y", "B X", "C Z"]
  putStrLn
    (
      "Score from lines with Rock vs Paper every round: "
      ++ show (scoreFromLines (\_ -> (Rock, Paper)) gameLines)
      ++ "; should be 24"
    )
