import Utils.Runner

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases [x] = 0
countIncreases (x:y:xs) = (if x < y then 1 else 0) + countIncreases (y:xs)

windowSum :: [Int] -> Int
windowSum xs = sum (take 3 xs)

countIncreasesWindow :: [Int] -> Int
countIncreasesWindow xz@(x:xs)
  | length xz < 3 = 0 
  | otherwise = (if windowSum xz < windowSum xs then 1 else 0) + countIncreasesWindow xs

main = do
  singleResult <- runWithInts countIncreases
  putStrLn ("Single increases: " ++ singleResult)
  windowResult <- runWithInts countIncreasesWindow
  putStrLn ("Window increases: " ++ windowResult)
