module Day11 (main) where

main :: IO ()
main = do
  -- Read file
  seats <- readInput "./inputFiles/day11.txt"
  --print $ part1 seats
  print $ part2 seats


readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

-- part 1, how many seats are occupied after the changes stabilize
part1 :: [String] -> Int
part1 seats =
  let updated = calcGeneration seats
   in if updated == seats
        then sum [1 | x <- updated, y <- x, y == '#']
        else part1 updated

-- part 2, how many seats are occupied after the changes stabilize
part2 :: [String] -> Int
part2 seats =
  let updated = calcGeneration2 seats
   in if updated == seats
        then sum [1 | x <- updated, y <- x, y == '#']
        else part2 updated

calcGeneration :: [String] -> [String]
calcGeneration seats = map (updateRow seats [0 .. length (head seats) - 1]) [0 .. length seats - 1]

evaluatePosition :: [String] -> (Int, Int) -> Char
evaluatePosition seats (x, y) =
  let pos = getPos seats (x, y)
   in if pos == '.'
        then '.'
        else
          let adjacentSeats = [getPos seats (x -1, y -1), getPos seats (x, y - 1), getPos seats (x + 1, y - 1), getPos seats (x -1, y), getPos seats (x + 1, y), getPos seats (x -1, y + 1), getPos seats (x, y + 1), getPos seats (x + 1, y + 1)]
              numberOccupiedAdjacent = sum [1 | x <- adjacentSeats, x == '#']
           in if numberOccupiedAdjacent == 0 && pos == 'L'
                then '#'
                else if numberOccupiedAdjacent >= 4 && pos == '#' then 'L' else pos

updateRow :: [String] -> [Int] -> Int -> String
updateRow seats rowIndexes y = map (\x -> evaluatePosition seats (x, y)) rowIndexes

getPos :: [String] -> (Int, Int) -> Char
getPos seats (x, y)
  | x < 0 || x >= length (head seats) = 'X'
  | y < 0 || y >= length seats = 'X'
  | otherwise = (seats !! y) !! x

calcGeneration2 :: [String] -> [String]
calcGeneration2 seats = map (updateRow2 seats [0 .. length (head seats) - 1]) [0 .. length seats - 1]

evaluatePosition2 :: [String] -> (Int, Int) -> Char
evaluatePosition2 seats currentPos =
  let pos = getPos seats currentPos
   in if pos == '.'
        then '.'
        else
          let adjacentSeats = [getPosLine seats currentPos (-1, -1),
                getPosLine seats currentPos (0, -1),
                getPosLine seats currentPos (1, - 1),
                getPosLine seats currentPos (-1, 0),
                getPosLine seats currentPos (1, 0),
                getPosLine seats currentPos (-1, 1),
                getPosLine seats currentPos (0, 1),
                getPosLine seats currentPos (1, 1)]
              numberOccupiedAdjacent = sum [1 | x <- adjacentSeats, x == '#']
           in if numberOccupiedAdjacent == 0 && pos == 'L'
                then '#'
                else if numberOccupiedAdjacent >= 5 && pos == '#' then 'L' else pos

updateRow2 :: [String] -> [Int] -> Int -> String
updateRow2 seats rowIndexes y = map (\x -> evaluatePosition2 seats (x, y)) rowIndexes

getPosLine :: [String] -> (Int, Int) -> (Int, Int) -> Char
getPosLine seats (x, y) (deltaX, deltaY) =
  let pos = getPos seats (x + deltaX, y + deltaY)
   in case pos of
        '#' -> '#'
        'L' -> 'L'
        '.' -> getPosLine seats (x + deltaX, y + deltaY) (deltaX, deltaY)
        'X' -> 'X'