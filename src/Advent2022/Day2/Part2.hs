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

lineToChars :: String -> (Char, Char)
lineToChars line =
  (
    head (head splitLine),
    head (last splitLine)
  )
  where splitLine = words line

-- charsToChoices :: (Char, Char) -> (Choice, Choice)
-- charsToChoices (oc, uc) = 
--   (
--     parseChoice opponentChoiceMap oc,
--     parseChoice userChoiceMap uc
--   )
--
-- gameScoreFromLines :: [String] -> Int
-- gameScoreFromLines = gameScore . map (charsToChoices . lineToChars)
--
-- run :: FilePath -> IO ()
-- run filePath = do
--   fileLines <- readLines filePath
--   let result = gameScoreFromLines fileLines
--   putStrLn ("Part 1 final score: " ++ show result)

-- test :: IO ()
-- test = do
--   putStrLn ("Parse X: " ++ show (parseChoice userChoiceMap 'X') ++ "; should be Rock")
--
--   putStrLn ("Line to chars (Y, C): " ++ show (lineToChars "C Y") ++ "; should be ('C','Y')")
--   putStrLn ("Chars to choices (Y, C): " ++ show (charsToChoices ('C', 'Y')) ++ "; should be (Scissors,Paper)")
--
--   let gameLines = ["A Y", "B X", "C Z"]
--   putStrLn ("Game from lines: " ++ show (gameScoreFromLines gameLines))
--
