import Utils.Runner
import Text.Read

readLine :: String -> Maybe Int
readLine l = readMaybe l

getCaloriesByElf :: [Maybe Int] -> [Int]
getCaloriesByElf [] = [0]
getCaloriesByElf (Nothing:xs) = 0 : getCaloriesByElf xs
getCaloriesByElf (Just x:xs) = (x + head rest) : tail rest
  where rest = getCaloriesByElf xs

main = do
  caloriesPerElf <- run (getCaloriesByElf . map readLine)
  let maxCalories = maximum caloriesPerElf
  putStrLn ("Part 1: Max calories for one elf: " ++ (show maxCalories))
