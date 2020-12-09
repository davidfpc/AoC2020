module Day9 (main) where

import Data.List (tails)

main :: IO ()
main = do
  -- Read file
  intructions <- readInput "./inputFiles/day9.txt"
  let part1Result = part1 ([], intructions)
  print part1Result
  let result = part2 part1Result [] intructions in print $ minimum result + maximum result

readInput :: FilePath -> IO [Int]
readInput file = do
  contents <- readFile file
  -- Convert the content to Int Array
  let numericLines = map read $ lines contents
  return numericLines

-- part 1, find the element that is not a sum of the previous 2 = 69316178
part1 :: ([Int], [Int]) -> Int
part1 (numbers, pipeline) =
  let x : xs = pipeline
      a = reverse $ take 26 (reverse numbers)
   in if length numbers < 27 || numberIsSumOfAny2 x numbers then part1 (a ++ [x], xs) else x

numberIsSumOfAny2 :: Int -> [Int] -> Bool
numberIsSumOfAny2 number currentStack = not $ null [x * y | x : xs <- tails currentStack, y <- xs, x + y == number]

-- part 2, find the group that, added, is equal to part1 result
part2 :: Int -> [Int] -> [Int] -> [Int]
part2 sumResult numbers pipeline =
  let x : xs = pipeline
      newNumbers = numbers ++ [x]
      numberSum = sum newNumbers
   in if numberSum == sumResult
        then newNumbers
        else if numberSum < sumResult then part2 sumResult newNumbers xs else part2 sumResult (tail numbers) pipeline
