module Day6 (main) where

import Data.List (intersect, union)

main :: IO ()
main = do
  -- Read file
  customsAnswers <- readInput "./inputFiles/day6.txt"
  print $ part1 customsAnswers
  print $ part2 customsAnswers

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

-- part 1, how many answers to the customs forms
part1 :: [String] -> Int
part1 input =
  let answers = foldl processInputPart1 ([], "") input
      answersPerGroup = fst answers ++ [snd answers]
   in sum $ map length answersPerGroup

-- part 2, how many answers to the customs forms chosen by all members of a group 
part2 :: [String] -> Int
part2 input =
  let answers = foldl processInputPart2 ([], ['a' .. 'z']) input
      answersPerGroup = fst answers ++ [snd answers]
   in sum $ map length answersPerGroup

processInputPart1 :: ([String], String) -> String -> ([String], String)
processInputPart1 (answers, currentGroupAnswers) line = case line of
  "" -> (answers ++ [currentGroupAnswers], [])
  _ -> (answers, currentGroupAnswers `union` line)

processInputPart2 :: ([String], String) -> String -> ([String], String)
processInputPart2 (answers, currentGroupAnswers) line = case line of
  "" -> (answers ++ [currentGroupAnswers], ['a' .. 'z'])
  _ -> (answers, currentGroupAnswers `intersect` line)
