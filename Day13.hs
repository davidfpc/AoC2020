module Day13 (main) where

import Data.List (sortBy)
import Debug.Trace (traceShow)
import Text.Parsec
  ( char,
    digit,
    many1,
    optional,
    parse,
    (<|>),
  )
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  -- Read file
  timestamp : buses <- readInput "./inputFiles/day13.txt"
  let parsedBuses = myRight $ parse parseInput "" (head buses)
  print $ part1 (read timestamp) (filter (/= (-1)) parsedBuses)
  print $ part2 parsedBuses 0 0 (head parsedBuses)

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

part1 :: Int -> [Int] -> Int
part1 currentTimestamp buses =
  let timings = sortBy (\(a, _) (b, _) -> compare a b) (map (\x -> ((divide currentTimestamp x * x) - currentTimestamp, x)) buses)
      (minutes, bus) = head timings
   in minutes * bus

part2 :: [Int] -> Int -> Int -> Int -> Int
part2 buses busIndex timestamp increment =
  let increaseIndex = busIndex + 1
   in if buses !! busIndex == -1
        then part2 buses increaseIndex timestamp increment
        else
          if mod (timestamp + busIndex) (buses !! busIndex) == 0
            then
              if increaseIndex == length buses
                then timestamp -- Found it!
                else traceShow ("calling Next with:" ++ show increaseIndex ++ ", " ++ show timestamp ++ ", " ++ show (myLCM buses increaseIndex)) part2 buses increaseIndex timestamp (myLCM buses increaseIndex) -- check next
            else -- no luck, increment
              part2 buses busIndex (timestamp + increment) increment

myLCM :: [Int] -> Int -> Int
myLCM buses index =
  let filteredBuses = filter (/= (-1)) (take index buses)
   in foldl lcm 1 filteredBuses

parseInput :: Parser [Int]
parseInput = do many1 (bus <|> noBusId)

bus :: Parser Int
bus = do
  busId <- many1 digit
  optional $ char ','
  return $ read busId

noBusId :: Parser Int
noBusId = do
  char 'x'
  optional $ char ','
  return (-1)

myRight (Right x) = x

-- integer divide, always rounding up
divide a b = ceiling (fromIntegral a / fromIntegral b)