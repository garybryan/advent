module Advent2022.Day19.Parsing
  ( parseLine,
    parseLines,
  )
where

import Advent2022.Day19.Base
import qualified Data.Map as Map
import qualified Data.Vector as V
import Lib.Parsing (intParser, parseOrError)
import Text.Parsec hiding (State)
import Text.Parsec.String

resourceIndices :: Map.Map String Int
resourceIndices = Map.fromList $ zip ["ore", "clay", "obsidian", "geode"] [0 ..]

blueprintNumParser :: Parser Int
blueprintNumParser = string "Blueprint " *> intParser <* string ": "

resourceParser :: Parser String
resourceParser = try (string "ore") <|> string "clay" <|> string "obsidian" <|> string "geode"

costParser :: Parser (Int, String)
costParser = (,) <$> intParser <* char ' ' <*> resourceParser

costsParser :: Parser [(Int, String)]
costsParser = costParser `sepBy1` string " and "

robotParser :: Parser (String, [(Int, String)])
robotParser =
  (,)
    <$> (string "Each " *> resourceParser)
    <*> (string " robot costs " *> costsParser <* char '.')

robotsParser :: Parser [(String, [(Int, String)])]
robotsParser = robotParser `sepBy1` char ' '

blueprintParser :: Parser (Int, [(String, [(Int, String)])])
blueprintParser = (,) <$> blueprintNumParser <*> robotsParser

parseLine :: String -> Blueprint
parseLine s = (num, costs)
  where
    (num, rs) = parseOrError blueprintParser s
    resCost (cost, resName) = (resourceIndices Map.! resName, cost)
    resCosts = map resCost
    makeCosts (_, cs) = V.replicate 4 0 V.// resCosts cs
    costs = V.fromList $ map makeCosts rs

parseLines :: [String] -> [Blueprint]
parseLines = map parseLine
