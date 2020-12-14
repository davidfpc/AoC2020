module Day14 (main) where

import Data.Bits (Bits ((.&.), (.|.)), bit, clearBit, setBit)
import Data.List (elemIndices)
import Data.Map (Map, empty, foldr, insert)
import Text.Parsec
  ( alphaNum,
    char,
    digit,
    many1,
    parse,
    string,
    (<|>),
  )
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  -- Read file
  input <- readInput "./inputFiles/day14.txt"
  let operations = rights $ map (parse parseInput "") input
  print $ part1 operations ("", empty) --we can pass "" since the first instruction is to set the mask
  print $ part2 operations ("", empty)

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

part1 :: [Either String (Int, Int)] -> (String, Map Int Int) -> Int
part1 (currentOperation : remaining) status =
  let newStatus = either (updateMask status) (setValueInMemory status) currentOperation
   in part1 remaining newStatus
part1 [] (_, memory) = Data.Map.foldr (+) 0 memory

part2 :: [Either String (Int, Int)] -> (String, Map Int Int) -> Int
part2 (currentOperation : remaining) status =
  let newStatus = either (updateMask status) (setValueInMemory2 status) currentOperation
   in part2 remaining newStatus
part2 [] (_, memory) = Data.Map.foldr (+) 0 memory

setValueInMemory2 :: (String, Map Int Int) -> (Int, Int) -> (String, Map Int Int)
setValueInMemory2 (mask, memory) (index, value) =
  let orMask = foldl (\x y -> setBit x (35 - y)) 0 (elemIndices '1' mask)
      maskedIndex = index .|. orMask
      newIndex = generateIndexes (elemIndices 'X' mask) maskedIndex
   in (mask, foldl (\x y -> insert y value x) memory newIndex)

generateIndexes :: [Int] -> Int -> [Int]
generateIndexes (x : xs) index =
  let indexWithPos0 = clearBit index (35 - x)
      indexWithPos1 = setBit index (35 - x)
   in generateIndexes xs indexWithPos0 ++ generateIndexes xs indexWithPos1
generateIndexes [] index = [index]

setValueInMemory :: (String, Map Int Int) -> (Int, Int) -> (String, Map Int Int)
setValueInMemory (mask, memory) (index, value) =
  let orMask = foldl (\x y -> setBit x (35 - y)) 0 (elemIndices '1' mask)
      andMask = foldl (\x y -> clearBit x (35 - y)) (bit 36 -1) (elemIndices '0' mask)
      maskedValue = (value .&. andMask) .|. orMask
   in (mask, insert index maskedValue memory)

updateMask :: (String, Map Int Int) -> String -> (String, Map Int Int)
updateMask (_, memory) updatedMask = (updatedMask, memory)

parseInput :: Parser (Either String (Int, Int))
parseInput = do
  char 'm'
  mask <|> memorySet

mask :: Parser (Either String (Int, Int))
mask = do
  string "ask = "
  maskValue <- many1 alphaNum
  return $ Left maskValue

memorySet :: Parser (Either String (Int, Int))
memorySet = do
  string "em["
  index <- many1 digit
  string "] = "
  value <- many1 digit
  return $ Right (read index, read value)

rights :: [Either a1 a2] -> [a2]
rights (Right x : xs) = x : rights xs
rights (_ : xs) = rights xs
rights [] = []