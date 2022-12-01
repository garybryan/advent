import Runner
import Text.Read

readLine :: String -> Maybe Int
readLine l = readMaybe l

caloriesByElf :: [Maybe Int] -> [Int]
caloriesByElf [] = [0]
caloriesByElf (Nothing:xs) = 0 : caloriesByElf xs
caloriesByElf (Just x:xs) = (x + head rest) : tail rest
  where rest = caloriesByElf xs

main = do
  result <- run (maximum . caloriesByElf . map readLine)
  putStrLn result
