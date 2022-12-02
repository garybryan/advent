module Advent2021.Day2 (run) where

import Lib.Read (readLines)

type Command = (String, Int)

type Vector = (Int, Int)

parseLine :: String -> Command
parseLine line = (head splitLine, read (last splitLine))
  where
    splitLine = words line

getCommandVector :: Command -> Vector
getCommandVector ("up", n) = (- n, 0)
getCommandVector ("down", n) = (n, 0)
getCommandVector ("forward", n) = (0, n)
getCommandVector (_, _) = (0, 0)

addVectors :: Vector -> Vector -> Vector
addVectors a b = (fst a + fst b, snd a + snd b)

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

run :: FilePath -> IO ()
run filePath = do
  fileLines <- readLines filePath
  let commands = map parseLine fileLines

  let result = getArea (getFinalPosition commands)
  putStrLn ("Part 1 result: " ++ show result)

  let resultAim = getArea (getFinalPositionAim commands)
  putStrLn ("Part 2 (with aim) result: " ++ show resultAim)
