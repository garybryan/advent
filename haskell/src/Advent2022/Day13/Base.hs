module Advent2022.Day13.Base
  ( Packet (..),
    Pair,
    parsePacket,
    isInOrder,
  )
where

import Text.Parsec
import Text.Parsec.String

data Packet = PInt Int | PPacket [Packet] deriving (Show, Eq)

type Pair = (Packet, Packet)

-- I think it's time to use a proper parsing library instead of all the DIY stuff.
-- TODO use Parsec for parseLines and parsePair too.
-- Focusing on solving the problem first, then making it nice.

packetIntParser :: Parser Packet
packetIntParser = PInt . read <$> many1 digit

packetListParser :: Parser Packet
packetListParser = PPacket <$> (packetParser <|> packetIntParser) `sepBy` char ','

packetParser :: Parser Packet
packetParser = between (char '[') (char ']') packetListParser

parsePacket :: String -> Packet
parsePacket s = case parse (packetParser <* eof) "" s of
  Left err -> error $ "parse error: " ++ show err
  Right p -> p

isInOrder :: Pair -> Maybe Bool
isInOrder (PInt l, PInt r) = case compare l r of
  LT -> Just True
  GT -> Just False
  EQ -> Nothing
isInOrder (p, PInt i) = isInOrder (p, PPacket [PInt i])
isInOrder (PInt i, p) = isInOrder (PPacket [PInt i], p)
isInOrder (PPacket [], PPacket []) = Nothing
isInOrder (PPacket [], _) = Just True
isInOrder (_, PPacket []) = Just False
isInOrder (PPacket (lp : lps), PPacket (rp : rps)) = case isInOrder (lp, rp) of
  Just b -> Just b
  Nothing -> isInOrder (PPacket lps, PPacket rps)

-- Sort packets by comparing with isInOrder. There might be a more efficient way?
instance Ord Packet where
  compare p1 p2 = case isInOrder (p1, p2) of
    Just True -> LT
    Just False -> GT
    Nothing -> EQ
