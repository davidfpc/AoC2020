module Day16 (main) where

import Data.List (elem)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (traceShow)
import Text.Parsec
  ( alphaNum,
    anyChar,
    char,
    digit,
    many1,
    parse,
    space,
    string,
    (<|>),
  )
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  -- Read file
  input <- readInput "./inputFiles/day16a.txt"
  let fields = map (rights . parse parseInput "") (takeWhile (not . null) input)
  let myTicket = mapCommaSeparatedToIntArray (input !! (length fields + 2)) -- skip the header and just get te specific line
  let otherTickets = map mapCommaSeparatedToIntArray (drop (length fields + 5) input)
  print $ day1 fields otherTickets
  print $ day2 fields otherTickets

day1 :: [(String, [Int])] -> [[Int]] -> Int
day1 fields otherTickets =
  let filteredTickets = map (filter (not . isFieldValid fields)) otherTickets
   in sum $ map sum filteredTickets

day2 :: [(String, [Int])] -> [[Int]] -> [[String]]
day2 fields otherTickets =
  let filteredTickets = filter (isTicketValid fields) otherTickets
      numberOfFields = length $ head filteredTickets
   in traceShow numberOfFields map (checkFields fields filteredTickets) [0 .. (numberOfFields -1)]

checkFields :: [(String, [Int])] -> [[Int]] -> Int -> [String]
checkFields (field : fields) tickets index = if validateField field tickets index then fst field : checkFields fields tickets index else checkFields fields tickets index
checkFields [] _ _ = []

validateField :: (String, [Int]) -> [[Int]] -> Int -> Bool
validateField field tickets index =
  let valuesToCheck = map (!! index) tickets
   in valuesAreValidForField field valuesToCheck

valuesAreValidForField :: (String, [Int]) -> [Int] -> Bool
valuesAreValidForField (a, possibleValues) (value : remValues) = value `elem` possibleValues && valuesAreValidForField (a, possibleValues) remValues
valuesAreValidForField _ [] = True

isTicketValid :: [(String, [Int])] -> [Int] -> Bool
isTicketValid fields (value : remValues) = isFieldValid fields value && isTicketValid fields remValues
isTicketValid _ [] = True

isFieldValid :: [(String, [Int])] -> Int -> Bool
isFieldValid (field : fields) value = foldr ((||) . elem value . snd) False fields

mapCommaSeparatedToIntArray :: String -> [Int]
mapCommaSeparatedToIntArray input = map read (splitOn "," input)

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

parseInput :: Parser (String, [Int])
parseInput = do
  fieldName <- many1 (alphaNum <|> space)
  string ": "
  first <- many1 digit
  char '-'
  second <- many1 digit
  string " or "
  third <- many1 digit
  char '-'
  fourth <- many1 digit
  let one = read first
      two = read second
      three = read third
      four = read fourth
  return (fieldName, [one .. two] ++ [three .. four])

rights :: Either a1 a2 -> a2
rights (Right x) = x