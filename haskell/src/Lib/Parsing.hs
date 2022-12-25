module Lib.Parsing
  ( intParser,
    pointParser,
    pointParser3d,
    parseOrError,
  )
where

import Text.Parsec
import Text.Parsec.String

intParser :: Parser Int
intParser = read <$> (((:) <$> char '-' <*> digits) <|> digits)
  where
    digits = many1 digit

intAndCommaParser :: Parser Int
intAndCommaParser = intParser <* char ','

pointParser :: Parser (Int, Int)
pointParser = (,) <$> intAndCommaParser <*> intParser

pointParser3d :: Parser (Int, Int, Int)
pointParser3d = (,,) <$> intAndCommaParser <*> intAndCommaParser <*> intParser

parseOrError :: Parser a -> String -> a
parseOrError p s = case parse p "" s of
  Left err -> error $ "Parse error: " ++ show err
  Right result -> result
