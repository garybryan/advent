module Lib.Parsing
  ( intParser,
    pointParser,
    parseOrError,
  )
where

import Text.Parsec
import Text.Parsec.String

intParser :: Parser Int
intParser = read <$> (((:) <$> char '-' <*> digits) <|> digits)
  where
    digits = many1 digit

pointParser :: Parser (Int, Int)
pointParser = (,) <$> (intParser <* char ',') <*> intParser

parseOrError :: Parser a -> String -> a
parseOrError p s = case parse p "" s of
  Left err -> error $ "Parse error: " ++ show err
  Right result -> result
