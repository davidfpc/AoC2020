module Day19 (main) where

import Data.Map (Map, empty, foldr, insert, lookup)
import Data.Maybe (fromJust)
import Data.Set (Set, fromList, member, size)
import Debug.Trace (trace, traceShow)
import Text.Parsec
  ( alphaNum,
    char,
    digit,
    eof,
    many1,
    optional,
    parse,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)
import Prelude hiding (lookup)

data Rule = Literal Char | List [Int] | Choice ([Int], [Int]) deriving (Show)

main :: IO ()
main = do
  -- Read file
  input <- readInput "./inputFiles/day19.txt"
  let rules = foldl (\a x -> let result = rights $ parse parseRule "" x in uncurry insert result a) empty (takeWhile (not . null) input)
      messages = drop (length rules + 1) input
      possibleSolutions = fromList $ createPossibleSolutions rules 0
  --print rules
  --print possibleSolutions
  print $ part1 messages possibleSolutions

part1 :: [String] -> Set String -> Int
part1 messages possibleSolutions = length $ filter (`member` possibleSolutions) messages

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

createPossibleSolutions :: Map Int Rule -> Int -> [String]
createPossibleSolutions ruleMap ruleId = let rule = fromJust $ lookup ruleId ruleMap in addSolution ruleMap rule

addSolution :: Map Int Rule -> Rule -> [String]
addSolution ruleMap (List rule) =
  let results : resultOther = map (createPossibleSolutions ruleMap) rule
   in foldl (\a b -> [x ++ y | x <- a, y <- b]) results resultOther
addSolution ruleMap (Choice rule) =
  let tmpFirstChoice = map (createPossibleSolutions ruleMap) (fst rule)
      firstChoice = if length tmpFirstChoice == 2 then [x ++ y | x <- head tmpFirstChoice, y <- last tmpFirstChoice] else head tmpFirstChoice
      tmpSecondChoice = map (createPossibleSolutions ruleMap) (snd rule)
      secondChoice = if length tmpSecondChoice == 2 then [x ++ y | x <- head tmpSecondChoice, y <- last tmpSecondChoice]  else head tmpSecondChoice
   in firstChoice ++ secondChoice
addSolution ruleMap (Literal rule) = [[rule]]

parseRule :: Parser (Int, Rule)
parseRule = do
  ruleNumber <- many1 digit
  string ": "
  rule <- try parseLiteral <|> try parseRuleChoice <|> try parseRuleList
  return (read ruleNumber, rule)

parseLiteral :: Parser Rule
parseLiteral = do
  char '"'
  literal <- alphaNum
  char '"'
  return $ Literal literal

parseRuleList :: Parser Rule
parseRuleList = do
  rules <- many1 parseRuleNumber
  eof
  return $ List rules

parseRuleChoice :: Parser Rule
parseRuleChoice = do
  rules <- many1 parseRuleNumber
  string "| "
  rules2 <- many1 parseRuleNumber
  return $ Choice (rules, rules2)

parseRuleNumber :: Parser Int
parseRuleNumber = do
  rule1 <- many1 digit
  optional $ char ' '
  return (read rule1)

rights :: Either a1 a2 -> a2
rights (Right x) = x
