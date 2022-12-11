module Advent2021.Day02 (run) where

import Data.Bifunctor (bimap)

type Command = (String, Int)

type Vector = (Int, Int)

parseLine :: String -> Command
parseLine line = (head splitLine, read $ last splitLine)
  where
    splitLine = words line

getCommandVector :: Command -> Vector
getCommandVector ("up", n) = (-n, 0)
getCommandVector ("down", n) = (n, 0)
getCommandVector ("forward", n) = (0, n)
getCommandVector (_, _) = (0, 0)

addVectors :: Vector -> Vector -> Vector
addVectors a = bimap (fst a +) (snd a +)

getFinalPosition :: [Command] -> Vector
getFinalPosition = foldr (addVectors . getCommandVector) (0, 0)

getArea :: Vector -> Int
getArea (x, y) = x * y

-- This can be done with foldl, but it gets messy, or did when I tried.
getFinalPositionAim :: [Command] -> Vector
getFinalPositionAim cs = aux (0, 0) 0 (map getCommandVector cs)
  where
    aux v _ [] = v
    aux (x, y) d ((dd, n) : cvs) = aux (x + n, y + d' * n) d' cvs
      where
        d' = d + dd

run :: [String] -> String
run ls = "Part 1 result: " ++ show result ++ "; Part 2 (with aim) result: " ++ show resultAim
  where
    commands = map parseLine ls
    result = getArea (getFinalPosition commands)
    resultAim = getArea (getFinalPositionAim commands)
