module Advent2022.Day16.Parsing
  ( parseLine,
    valveVectorAndStartIndexFromLines,
  )
where

import Advent2022.Day16.Base (NamedValve, Valve (..), ValveName, Valves)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Lib.Parsing (intParser, parseOrError)
import Text.Parsec
import Text.Parsec.String

valveNameParser :: Parser ValveName
valveNameParser = many letter

sourceValveParser :: Parser ValveName
sourceValveParser = string "Valve " *> valveNameParser

flowParser :: Parser Int
flowParser = string " has flow rate=" *> intParser

destValvesParser :: Parser [ValveName]
destValvesParser = valveNameParser `sepBy` string ", "

valveParser :: Parser (Int, [String])
valveParser =
  (,)
    <$> flowParser
    <*> ( string "; tunnel"
            *> (string "s lead to valves " <|> string " leads to valve ")
            *> destValvesParser
        )

lineParser :: Parser NamedValve
lineParser = (,) <$> sourceValveParser <*> valveParser

parseLine :: String -> NamedValve
parseLine = parseOrError lineParser

-- Store valves by index rather than name, so they can be used in bit vectors.
-- Valve "AA" isn't necessarily the first in the input, so we need to be able
-- to find its index in order to know where to start.
valveVectorAndStartIndex :: [NamedValve] -> (Valves, Int)
valveVectorAndStartIndex namesAndVals = (valves, index "AA")
  where
    nameIndices = Map.fromList $ zip (map fst namesAndVals) [0 ..]
    index name = fromMaybe (error $ "Unknown valve: " ++ name) $ Map.lookup name nameIndices
    entry (rate, adjNames) = Valve rate (map index adjNames)
    valves = V.fromList $ map (entry . snd) namesAndVals

valveVectorAndStartIndexFromLines :: [String] -> (Valves, Int)
valveVectorAndStartIndexFromLines = valveVectorAndStartIndex . map parseLine
