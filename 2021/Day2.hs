import Utils.Runner

type Command = (String, Int)
type Vector = (Int, Int)

parseLine :: String -> Command
parseLine line = (head splitLine, read (last splitLine))
  where splitLine = words line

getCommandVector :: Command -> Vector
getCommandVector ("up", n)      = (-n, 0)
getCommandVector ("down", n)    = (n, 0)
getCommandVector ("forward", n) = (0, n)

addVectors :: Vector -> Vector -> Vector
addVectors a b = (fst a + fst b, snd a + snd b)

getFinalPosition :: [Command] -> Vector
getFinalPosition cs = foldr addVectors (0, 0) (map getCommandVector cs)

getArea :: Vector -> Int
getArea (x, y) = x * y

main = do
  result <- run (getArea . getFinalPosition . map parseLine)
  putStrLn (show result)
