module Lib.Parsing
  ( intParser,
  )
where

import Text.Parsec
import Text.Parsec.String

intParser :: Parser Int
intParser = read <$> many1 digit
