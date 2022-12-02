module Advent2022.Day2 (main, run) where

import qualified Data.Map as Map

import Lib.Read (readLines)

data Choice = Rock | Paper | Scissors deriving (Eq, Ord, Show)

choiceScore :: Choice -> Int
choiceScore Rock     = 1
choiceScore Paper    = 2
choiceScore Scissors = 3

-- Choices define a circular ordering: P beats R, S beats P, R beats S.
-- TODO could use a circular ordering class?
compareChoices :: Choice -> Choice -> Ordering
compareChoices Scissors Rock  = LT
compareChoices Rock Scissors  = GT
compareChoices c1 c2          = compare c1 c2

resultScore :: Choice -> Choice -> Int
resultScore oc uc = case compareChoices uc oc of
  GT -> 6
  EQ -> 3
  LT -> 0

roundScore :: Choice -> Choice -> Int
roundScore oc uc = resultScore oc uc + choiceScore uc

gameScore :: [(Choice, Choice)] -> Int
gameScore = sum . map (\(oc, uc) -> roundScore oc uc)

type ChoiceParseMap = Map.Map Char Choice

userChoiceMap :: ChoiceParseMap
userChoiceMap = Map.fromList
  [
    ('X', Rock),
    ('Y', Paper),
    ('Z', Scissors)
  ]

opponentChoiceMap :: ChoiceParseMap
opponentChoiceMap = Map.fromList
  [
    ('A', Rock),
    ('B', Paper),
    ('C', Scissors)
  ]

parseChoice :: ChoiceParseMap -> Char -> Choice
parseChoice cpm c = case Map.lookup c cpm of
  Nothing     -> error ("Invalid choice char: " ++ show c)
  Just choice -> choice

lineToChars :: String -> (Char, Char)
lineToChars line =
  (
    head (head splitLine),
    head (last splitLine)
  )
  where splitLine = words line

charsToChoices :: (Char, Char) -> (Choice, Choice)
charsToChoices (oc, uc) = 
  (
    parseChoice opponentChoiceMap oc,
    parseChoice userChoiceMap uc
  )

gameScoreFromLines :: [String] -> Int
gameScoreFromLines = gameScore . map (charsToChoices . lineToChars)

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let result = gameScoreFromLines fileLines
  putStrLn ("Part 1 final score: " ++ show result)

main :: IO ()
main = do
  -- TODO: learn use a proper unit test system for Haskell! This is very DIY.

  putStrLn ("Rock score: " ++ show (choiceScore Rock) ++ "; should be 1")
  putStrLn ("Scissors score: " ++ show (choiceScore Scissors) ++ "; should be 3")

  putStrLn ("Compare Rock to Scissors: " ++ show (compareChoices Rock Scissors) ++ "; should be GT")
  putStrLn ("Compare Rock to Paper: " ++ show (compareChoices Rock Paper) ++ "; should be LT")
  putStrLn ("Compare Rock to Rock: " ++ show (compareChoices Rock Rock) ++ "; should be EQ")

  putStrLn ("Rock vs scissors result score: " ++ show (resultScore Rock Scissors) ++ "; should be 6")
  putStrLn ("Paper vs paper result score: " ++ show (resultScore Paper Paper) ++ "; should be 3")
  putStrLn ("Paper vs scissors result score: " ++ show (resultScore Paper Scissors) ++ "; should be 0")

  putStrLn ("Paper vs rock round score: " ++ show (roundScore Paper Rock) ++ "; should be 8")
  putStrLn ("Rock vs paper round score: " ++ show (roundScore Rock Paper) ++ "; should be 1")
  putStrLn ("Scissors vs scissors round score: " ++ show (roundScore Scissors Scissors) ++ "; should be 6")

  let game = [(Paper, Rock), (Rock, Paper), (Scissors, Scissors)]
  putStrLn ("Game score: " ++ show (gameScore game) ++ "; should be 15")

  putStrLn ("Parse B: " ++ show (parseChoice opponentChoiceMap 'B') ++ "; should be Paper")
  putStrLn ("Parse X: " ++ show (parseChoice userChoiceMap 'X') ++ "; should be Rock")

  -- TODO test for error
  -- putStrLn ("Parse unknown: " ++ show (parseChoice opponentChoiceMap 'E') ++ "; should raise error")

  putStrLn ("Line to chars (Y, C): " ++ show (lineToChars "C Y") ++ "; should be ('C','Y')")
  putStrLn ("Chars to choices (Y, C): " ++ show (charsToChoices ('C', 'Y')) ++ "; should be (Scissors,Paper)")

  let gameLines = ["A Y", "B X", "C Z"]
  putStrLn ("Game from lines: " ++ show (gameScoreFromLines gameLines))

