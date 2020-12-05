module Day5 (main) where

import Data.List (sort)

main :: IO ()
main = do
  -- Read file
  boardingPasses <- readInput "./inputFiles/day5.txt"
  print $ part1 boardingPasses
  print $ part2 boardingPasses

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

-- part 1, what is the higher seat
part1 :: [String] -> Int
part1 = maximum . map (calcId . binarySearch)

-- part 2, what is my seat
part2 :: [String] -> Int
part2 boardingPasses =
  let seats = [8 .. ((8 * 126) + 7)]
      bpass = sort $ map (calcId . binarySearch) boardingPasses
   in filterMissingSeats $ filter (`notElem` bpass) seats

-- My seat is the only one that has an occupied before and after
filterMissingSeats :: [Int] -> Int
filterMissingSeats seats = head $ filter (\x -> x + 1 `notElem` seats && x -1 `notElem` seats) seats

-- Binary search to get the seat and row based on the arline binary space partitioning map
binarySearch :: String -> (Int, Int)
binarySearch bpass =
  let rowMap = take 7 bpass
      seatMap = drop 7 bpass
      row = fst $ foldl calcPos (0, 127) rowMap
      seat = fst $ foldl calcPos (0, 7) seatMap
   in (row, seat)

-- calc the position (main part of the binary search).
-- Reuse the function for row and collumn just by having the different map chars
calcPos :: (Int, Int) -> Char -> (Int, Int)
calcPos (minPos, maxPos) rowChar =
  let dif = divide (maxPos - minPos) 2.0
   in case rowChar of
        'F' -> (minPos, maxPos - dif)
        'B' -> (minPos + dif, maxPos)
        'L' -> (minPos, maxPos - dif)
        'R' -> (minPos + dif, maxPos)

-- calculate the id of the seat
calcId :: (Int, Int) -> Int
calcId (row, seat) = row * 8 + seat

-- integer divide, always rounding up
divide a b = ceiling (fromIntegral a / b)
