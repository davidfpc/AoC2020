module Day7 (main) where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Text.Parsec
  ( char,
    digit,
    letter,
    many,
    many1,
    optional,
    parse,
    string,
    (<|>),
  )
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  -- Read file
  bagRules <- readInput "./inputFiles/day7.txt"
  let parsedInput = rights $ map (parse parseInput "") bagRules
  let parsedInput2 = rights $ map (parse parseInput2 "") bagRules
  print $ part1 parsedInput
  print $ part2 parsedInput2

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

-- part 1, how many bags can contain the gold one
part1 :: [(String, [String])] -> Int
part1 input = length $ nub $findParents input "shiny gold"

-- part 2, how many bags can the gold one have inside
part2 :: [(String, [(Int, String)])] -> Int
part2 input = findChildren input ("shiny gold", 1) - 1 -- Why do I need to subtract one? something is wrong here!!!!

findChildren :: [(String, [(Int, String)])] -> (String, Int) -> Int
findChildren input (bag, count) =
  let children = map (\x -> (snd x, fst x * count)) (myLookup bag input)
   in if null children then count else count + (sum $ flatmap2 (findChildren input) children)

myLookup i xs = fromMaybe [] (lookup i xs)

flatmap2 f (x : xs) = f x : flatmap2 f xs
flatmap2 _ [] = []

findParents :: [(String, [String])] -> String -> [String]
findParents input bag =
  let parents = [fst x | x <- input, bag `elem` snd x]
   in flatmap (findParents input) parents

flatmap f (x : xs) = [x] ++ f x ++ flatmap f xs
flatmap _ [] = []

parseInput :: Parser (String, [String])
parseInput = do
  adjective <- many1 letter
  char ' '
  colour <- many1 letter
  string " bags contain "
  innerBags <- many bagContents <|> emptyBagContents
  char '.'
  return (adjective ++ " " ++ colour, innerBags)

emptyBagContents :: Parser [String]
emptyBagContents = do
  string "no other bags"
  return []

bagContents :: Parser String
bagContents = do
  _ <- digit
  char ' '
  adjective <- many1 letter
  char ' '
  colour <- many1 letter
  string " bag"
  optional $ char 's'
  optional $ string ", "
  return (adjective ++ " " ++ colour)

parseInput2 :: Parser (String, [(Int, String)])
parseInput2 = do
  adjective <- many1 letter
  char ' '
  colour <- many1 letter
  string " bags contain "
  innerBags <- many bagContents2 <|> emptyBagContents2
  char '.'
  return (adjective ++ " " ++ colour, innerBags)

emptyBagContents2 :: Parser [(Int, String)]
emptyBagContents2 = do
  string "no other bags"
  return []

bagContents2 :: Parser (Int, String)
bagContents2 = do
  count <- many1 digit
  char ' '
  adjective <- many1 letter
  char ' '
  colour <- many1 letter
  string " bag"
  optional $ char 's'
  optional $ string ", "
  return (read count, adjective ++ " " ++ colour)

rights (Right x : xs) = x : rights xs
rights (_ : xs) = rights xs
rights [] = []
