module Day4 (main) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  -- Read file
  passports <- readInput "./inputFiles/day4.txt"
  print $ part1 passports
  print $ part2 passports

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

part1 :: [String] -> Int
part1 input =
  let passports = foldl processInput ([], []) input
   in length $ filter validPassport (fst passports ++ [snd passports])

part2 :: [String] -> Int
part2 input =
  let passports = foldl processInputAndValidate ([], []) input
   in length $ filter validPassport (fst passports ++ [snd passports])

validPassport :: [String] -> Bool
validPassport passport = notElem "invalid" passport && (length passport == 8 || length passport == 7 && notElem "cid" passport)

processInput :: ([[String]], [String]) -> String -> ([[String]], [String])
processInput (passports, currentPassport) line = case line of
  "" -> (passports ++ [currentPassport], [])
  _ -> (passports, currentPassport ++ map readKey (splitOn " " line))

processInputAndValidate :: ([[String]], [String]) -> String -> ([[String]], [String])
processInputAndValidate (passports, currentPassport) line = case line of
  "" -> (passports ++ [currentPassport], [])
  _ -> (passports, currentPassport ++ map readKeyAndValidate (splitOn " " line))

readKey :: String -> String
readKey input = key where [key, _] = splitOn ":" input

readKeyAndValidate :: String -> String
readKeyAndValidate input =
  let [tmp_key, value] = splitOn ":" input
   in validateKey tmp_key value

--byr (Birth Year) - four digits; at least 1920 and at most 2002.
--iyr (Issue Year) - four digits; at least 2010 and at most 2020.
--eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
--hgt (Height) - a number followed by either cm or in:
--    If cm, the number must be at least 150 and at most 193.
--    If in, the number must be at least 59 and at most 76.
--hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
--ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
--pid (Passport ID) - a nine-digit number, including leading zeroes.
--cid (Country ID) - ignored, missing or not.

-- HAMMER TIME -> we make it invalid by saying than an invalid key is a cid - therefore, the 8/7without cid conditions will never be reached
validateKey :: String -> String -> String
validateKey key value = case key of
  "byr" -> let number = read value in if number >= 1920 && number <= 2002 then key else "invalid"
  "iyr" -> let number = read value in if number >= 2010 && number <= 2020 then key else "invalid"
  "eyr" -> let number = read value in if number >= 2020 && number <= 2030 then key else "invalid"
  "hgt" ->
    let number = read $ takeWhile isDigit value
     in if reverse (take 2 (reverse value)) == "cm" && number >= 150 && number <= 193
          || reverse (take 2 (reverse value)) == "in" && number >= 59 && number <= 76
          then key
          else "invalid"
  "hcl" -> if head value == '#' && all isCharHexa (tail value) && length value == 7 then key else "invalid"
  "ecl" -> if value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] then key else "invalid"
  "pid" -> if length value == 9 && all isDigit value then key else "invalid"
  "cid" -> "cid"
  _ -> "invalid"

isCharHexa bla = bla `elem` "0123456789abcdefABCDEF"