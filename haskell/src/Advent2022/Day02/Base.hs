module Advent2022.Day02.Base
  ( Choice (..),
    ChoiceParseMap,
    ParseMap,
    Round,
    gameScore,
    lineToChars,
    opponentChoiceMap,
    parseChar,
    scoreFromLines,
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
choiceScore = (+ 1) . fromEnum

compareCirc :: (Bounded a, Ord a) => a -> a -> Ordering
compareCirc c1 c2
  | c1 == minBound && c2 == maxBound = GT
  | c1 == maxBound && c2 == minBound = LT
  | otherwise = compare c1 c2

-- Represent a single round as (opponent's choice, user's choice).
type Round = (Choice, Choice)

resultScore :: Round -> Int
resultScore (oc, uc) = case compareCirc uc oc of
  GT -> 6
  EQ -> 3
  LT -> 0

roundScore :: Round -> Int
roundScore (oc, uc) = resultScore (oc, uc) + choiceScore uc

gameScore :: [Round] -> Int
gameScore = sum . map roundScore

type ParseMap a = Map.Map Char a

type ChoiceParseMap = ParseMap Choice

opponentChoiceMap :: ChoiceParseMap
opponentChoiceMap =
  Map.fromList
    [ ('A', Rock),
      ('B', Paper),
      ('C', Scissors)
    ]

parseChar :: ParseMap a -> Char -> a
parseChar cpm c = case Map.lookup c cpm of
  Nothing -> error ("Invalid char: " ++ show c)
  Just val -> val

lineToChars :: String -> (Char, Char)
lineToChars line =
  ( head (head splitLine),
    head (last splitLine)
  )
  where
    splitLine = words line

scoreFromLines :: ((Char, Char) -> Round) -> [String] -> Int
scoreFromLines charsFn = gameScore . map (charsFn . lineToChars)
