module Day2 (main) where

import Data.List.Split (splitOneOf)

type PasswordData = (Int, Int, Char, String)

main :: IO ()
main = do
  -- Read file
  passwords <- readInput "./inputFiles/day2.txt"
  print $ part1 passwords
  print $ part2 passwords

-- Notice the IO [Int] -> this is needed because of the error handling?????
readInput :: FilePath -> IO [PasswordData]
readInput file = do
  -- Read File (IO String, so when doing "<-" whe are executing the IO action, so contents will be a String and not an IO String)
  contents <- readFile file
  -- Convert the content to PasswordRecord Array
  let input = map parseLine $ lines contents
  return input

-- filter the passwords based on the part1Filter, and count the results
part1 :: [PasswordData] -> Int
part1 passwords = length $ filter part1Filter passwords

-- filter the passwords based on the part2Filter, and count the results
part2 :: [PasswordData] -> Int
part2 passwords = length $ filter part2Filter passwords

-- filter the password, to only contain the charPassword chars and the validate if the number of occurrences is between the given values
part1Filter :: PasswordData -> Bool
part1Filter (minOccurs, maxOccurs, charPassword, pass) = n >= minOccurs && n <= maxOccurs
  where
    n = length $ filter (== charPassword) pass

-- get the p1 and p2 positions of the password, and check if they are the same as charPassword. As only one of them can contain it, validate that the comparation results are diferent (xor)
part2Filter :: PasswordData -> Bool
part2Filter (p1, p2, charPassword, pass) = (c1 == charPassword) /= (c2 == charPassword)
  where
    c1 = pass !! (p1 -1)
    c2 = pass !! (p2 -1)

-- parse the input, converting to PasswordData
parseInput :: [String] -> PasswordData
parseInput input = (n1, n2, c, pass)
  where
    n1 = read $ head input
    n2 = read $ input !! 1
    c = head $ input !! 2
    pass = input !! 4

-- parse the line and convert it to PasswordData
parseLine :: String -> PasswordData
parseLine input = parseInput splited
  where
    splited = splitOneOf "- :" input
