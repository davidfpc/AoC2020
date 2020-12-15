module Day15 (main) where

import Data.List.Split (splitOn)
import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
  -- Read file
  initialNumbers <- readInput "./inputFiles/day15.txt"
  let numberOfElements = length initialNumbers + 1
  let numberMap = fromList $ zip initialNumbers (Prelude.map (: []) [1 .. numberOfElements])
  print $ part1 (numberMap, last initialNumbers, numberOfElements)
  print $ part2 (numberMap, last initialNumbers, numberOfElements)

readInput :: FilePath -> IO [Int]
readInput file = do
  contents <- readFile file
  return $ Prelude.map read (splitOn "," contents)

part1 :: (Map Int [Int], Int, Int) -> Int
part1 turn = secondElement $ until (\x -> thirdElement x == 2021) calcTurn turn

part2 :: (Map Int [Int], Int, Int) -> Int
part2 turn = secondElement $ until (\x -> thirdElement x == 30000001) calcTurn turn

secondElement (_, a, _) = a

thirdElement (_, _, a) = a

calcTurn :: (Map Int [Int], Int, Int) -> (Map Int [Int], Int, Int)
calcTurn (numbers, previousNumber, turn) =
  let mapEntryPrev = fromJust $ Data.Map.lookup previousNumber numbers
      nextNumber = if length mapEntryPrev == 1 then 0 else - foldl (flip (-)) 0 mapEntryPrev
      mapEntry = Data.Map.lookup nextNumber numbers
   in (if isJust mapEntry then insert nextNumber [turn, head $ fromJust mapEntry] numbers else insert nextNumber [turn] numbers, nextNumber, turn + 1)
