module Day10 (main) where

import Data.List (sort, tails)

main :: IO ()
main = do
  -- Read file
  adapters <- readInput "./inputFiles/day10.txt"
  let differences = part1 adapters
  print $ (length $ filter (== 1) differences) * (length $ filter (== 3) differences)
  print differences
  print $ part2 differences (1, 0)

readInput :: FilePath -> IO [Int]
readInput file = do
  contents <- readFile file
  -- Convert the content to Int Array
  let numericLines = map read $ lines contents
  return numericLines

-- part 1, what is the multiplication between the 1 differences and 3 differences
part1 :: [Int] -> [Int]
part1 adaptors =
  let sorted = sort adaptors
      deviceJoltage = 3 + last sorted
      sortedWithDevice = [0] ++ sorted ++ [deviceJoltage]
      differences = [y - x | x : xs <- tails sortedWithDevice, y <- if length xs > 0 then [head xs] else []]
   in differences --

part2 :: [Int] -> (Int, Int) -> Int
part2 differences (currentCount, currentOnes) =
  if null differences
    then currentCount
    else
      let x : xs = differences
       in case x of
            1 -> part2 xs (currentCount, currentOnes + 1)
            3 -> part2 xs (currentCount * (if currentOnes == 2 then 2 else if currentOnes == 3 then 4 else if currentOnes == 4 then 7 else 1), 0)

-- 4 ones - 7 combinations
-- A B C D
-- A B   D
-- A   C D
-- A     D
--   B C D
--   B   D
--     C D

-- 3 ones - 4 combinations
-- A B C
-- A   C
--   B C
--     C

--2 ones - 2 combinations
-- A B
--   B