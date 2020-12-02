module Day2 (main) where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  -- Read file
  passwords <- readInput "./inputFiles/day2.txt"
  print $ part1 passwords
  print $ part2 passwords

-- Notice the IO [Int] -> this is needed because of the error handling?????
readInput :: FilePath -> IO [(Int, Int, Char, String)]
readInput file = do
  -- Read File (IO String, so when doing "<-" whe are executing the IO action, so contents will be a String and not an IO String)
  contents <- readFile file
  -- Convert the content to PasswordRecord Array
  let input = map parseLine $ lines contents
  return input

part1 :: [(Int, Int, Char, String)] -> Int
part1 passwords = length $ filter part1FilterFunc passwords

part2 :: [(Int, Int, Char, String)] -> Int
part2 passwords = length $ filter part2FilterFunc passwords

part1FilterFunc :: (Int, Int, Char, String) -> Bool
part1FilterFunc (minOccurs, maxOccurs, charPassword, passwordText) = do
  let countChars = length $ filter (== charPassword) passwordText
  (countChars >= minOccurs) && (countChars <= maxOccurs)

part2FilterFunc :: (Int, Int, Char, String) -> Bool
part2FilterFunc (index1, index2, charPassword, passwordText) = do
  let occur1 = index1 - 1
      occur2 = index2 -1
  (charPassword == (passwordText !! occur1) || charPassword == (passwordText !! occur2)) && ((passwordText !! occur1) /= (passwordText !! occur2))

parseLine :: String -> (Int, Int, Char, String)
parseLine input = do
  let splited = splitOn ": " input
  let password = last splited
  let occurences = head $ splitOn " " $ head splited
  let character = head $ last $ splitOn " " $ head splited
  let minOccurs = read $ head $ splitOn "-" occurences
  let maxOccurs = read $ last $ splitOn "-" occurences
  (minOccurs, maxOccurs, character, password)
