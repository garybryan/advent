import Runner

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [x] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)


main = do
  run (show . countIncreases)
