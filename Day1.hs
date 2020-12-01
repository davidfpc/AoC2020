module Day1(day1) where

import Data.List (tails)

day1 :: IO ()
day1 = do
  -- Read file
  expenses <- readInput "./inputFiles/day1.txt"
  print . part1 $ expenses
  print $ part2 expenses

-- Notice the IO [Int] -> this is needed because of the error handling?????
readInput :: FilePath -> IO [Int]
readInput file = do
  -- Read File (IO String, so when doing "<-" whe are executing the IO action, so contents will be a String and not an IO String)
  contents <- readFile file
  -- Convert the content to Int Array
  let numericLines = map read $ lines contents
  return numericLines

-- make tupples from the list, filtering by the sum, and multiplying the result
part1 :: [Int] -> Int
part1 expenses = head [x * y | x : xs <- tails expenses, y <- xs, x + y == 2020]

-- make tupples from the list, filtering by the sum, and multiplying the result
part2 :: [Int] -> Int
part2 expenses = head [x * y * z | x : xs <- tails expenses, y : ys <- tails xs, z <- ys, x + y + z == 2020]