module Advent2022.Day13.Base
  ( Packet (..),
    Pair,
    parsePacket,
    comparePackets,
  )
where

import Lib.Parsing (intParser, parseOrError)
import Text.Parsec
import Text.Parsec.String

data Packet = PInt Int | PPacket [Packet] deriving (Show, Eq)

type Pair = (Packet, Packet)

-- I think it's time to use a proper parsing library instead of all the DIY stuff.
-- TODO use Parsec for parseLines and parsePair too.
-- Focusing on solving the problem first, then making it nice.

packetIntParser :: Parser Packet
packetIntParser = PInt <$> intParser

packetListParser :: Parser Packet
packetListParser = PPacket <$> (packetParser <|> packetIntParser) `sepBy` char ','

packetParser :: Parser Packet
packetParser = between (char '[') (char ']') packetListParser

parsePacket :: String -> Packet
parsePacket = parseOrError packetParser

comparePackets :: Packet -> Packet -> Ordering
comparePackets (PInt l) (PInt r) = compare l r
comparePackets p (PInt i) = comparePackets p (PPacket [PInt i])
comparePackets (PInt i) p = comparePackets (PPacket [PInt i]) p
comparePackets (PPacket []) (PPacket []) = EQ
comparePackets (PPacket []) _ = LT
comparePackets _ (PPacket []) = GT
comparePackets (PPacket (lp : lps)) (PPacket (rp : rps)) = case comparePackets lp rp of
  EQ -> comparePackets (PPacket lps) (PPacket rps)
  result -> result

-- Sort packets by comparing with comparePackets. There might be a more efficient way?
instance Ord Packet where
  compare = comparePackets
