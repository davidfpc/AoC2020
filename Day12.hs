module Day12 (main) where

import Text.Parsec
  ( digit,
    letter,
    many1,
    parse,
  )
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  -- Read file
  intructions <- readInput "./inputFiles/day12.txt"
  let parsedIntructions = rights $ map (parse parseInput "") intructions
  print $ part1 parsedIntructions
  print $ part2 parsedIntructions

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

--     0
--  3 -|- 1
--     2
-- part 1, what is the Manhattan distance between the starting point and the final point
part1 :: [(Char, Int)] -> Int
part1 instuctions =
  let (_, (x, y)) = foldl p1ProcessIntruction (1, (0, 0)) instuctions
   in abs x + abs y

p1ProcessIntruction :: (Int, (Int, Int)) -> (Char, Int) -> (Int, (Int, Int))
p1ProcessIntruction (direction, (x, y)) (operation, value) = case operation of
  'N' -> (direction, (x + value, y))
  'S' -> (direction, (x - value, y))
  'E' -> (direction, (x, y + value))
  'W' -> (direction, (x, y - value))
  'L' -> p1Rotate (direction, (x, y)) (operation, value)
  'R' -> p1Rotate (direction, (x, y)) (operation, value)
  'F' -> p1Forward (direction, (x, y)) value

p1Rotate :: (Int, (Int, Int)) -> (Char, Int) -> (Int, (Int, Int))
p1Rotate (direction, coordinates) (operation, value) =
  let rotation = if operation == 'L' then -1 else 1
   in (mod (direction + (rotation * div value 90)) 4, coordinates)

p1Forward :: (Int, (Int, Int)) -> Int -> (Int, (Int, Int))
p1Forward (direction, coordinates) value = case direction of
  0 -> p1ProcessIntruction (direction, coordinates) ('N', value)
  1 -> p1ProcessIntruction (direction, coordinates) ('E', value)
  2 -> p1ProcessIntruction (direction, coordinates) ('S', value)
  3 -> p1ProcessIntruction (direction, coordinates) ('W', value)

-- part 2, what is the Manhattan distance between the starting point and the final point with the correct instructions
part2 :: [(Char, Int)] -> Int
part2 instuctions =
  let (_, (x, y)) = foldl p2ProcessIntruction ((10, 1), (0, 0)) instuctions
   in abs x + abs y

p2ProcessIntruction :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
p2ProcessIntruction ((dirX, dirY), (x, y)) (operation, value) = case operation of
  'N' -> ((dirX, dirY + value), (x, y))
  'S' -> ((dirX, dirY - value), (x, y))
  'E' -> ((dirX + value, dirY), (x, y))
  'W' -> ((dirX - value, dirY), (x, y))
  'L' -> p2Rotate ((dirX, dirY), (x, y)) value 1
  'R' -> p2Rotate ((dirX, dirY), (x, y)) value (-1)
  'F' -> ((dirX, dirY), (x + (dirX * value), y + (dirY * value)))

p2Rotate :: ((Int, Int), (Int, Int)) -> Int -> Int -> ((Int, Int), (Int, Int))
p2Rotate ((dirX, dirY), coordinates) value invert =
  let newCoordinates = ((dirY * (- invert), dirX * invert), coordinates)
   in if value > 90 then p2Rotate newCoordinates (value - 90) invert else newCoordinates

parseInput :: Parser (Char, Int)
parseInput = do
  operator <- letter
  arg <- many1 digit
  return (operator, read arg)

rights :: [Either a1 a2] -> [a2]
rights (Right x : xs) = x : rights xs
rights (_ : xs) = rights xs
rights [] = []