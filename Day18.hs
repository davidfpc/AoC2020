module Day18 (main) where

import Data.Bits (Bits ((.&.), (.|.)), bit, clearBit, setBit)
import Data.Char (digitToInt, isDigit)
import Data.List (elemIndices)
import Data.Map (Map, empty, foldr, insert)
import Debug.Trace (trace, traceShow)
import Text.Parsec
  ( alphaNum,
    char,
    digit,
    letter,
    many1,
    oneOf,
    optional,
    parse,
    string,
    (<|>),
  )
import Text.Parsec.String (Parser)

data Tree = Node Tree Char Tree | Leaf Int

main :: IO ()
main = do
  -- Read file
  operations <- readInput "./inputFiles/day18.txt"
  let spacelessOps = map (filter (/= ' ')) operations
  print $ part1 spacelessOps 0 '('
  print $ part2 spacelessOps 0 '('

part1 :: [String] -> Int -> Char -> Int
part1 exprs acc op = sum $ map (\x -> fst $ calcExpr x acc op) exprs

part2 :: [String] -> Int -> Char -> Int
part2 exprs acc op = sum $ map (\x -> fst $ calcExpr2 x acc op) exprs

calcExpr :: String -> Int -> Char -> (Int, String)
calcExpr (x : expr) acc op =
  if isDigit x
    then calcOp expr (digitToInt x) acc op
    else case x of
      '+' -> calcExpr expr acc x
      '*' -> calcExpr expr acc x
      '(' ->
        let innerBlock = calcExpr expr 0 '('
         in calcOp (snd innerBlock) (fst innerBlock) acc op
      ')' -> (acc, expr)
calcExpr [] acc _ = (acc, [])

calcOp :: String -> Int -> Int -> Char -> (Int, String)
calcOp expr x acc op = case op of
  '+' -> calcExpr expr (acc + x) '('
  '*' -> calcExpr expr (acc * x) '('
  _ -> calcExpr expr x '('

calcExpr2 :: String -> Int -> Char -> (Int, String)
calcExpr2 (x : expr) acc op =
  if isDigit x
    then calcOp2 expr (digitToInt x) acc op
    else case x of
      '+' -> calcExpr2 expr acc x
      '*' -> let innerBlock = calcExpr2 expr 0 '('
              in calcOp2 (')':snd innerBlock) (fst innerBlock) acc '*'
      '(' ->
        let innerBlock = calcExpr2 expr 0 '('
         in calcOp2 (snd innerBlock) (fst innerBlock) acc op
      ')' -> (acc, expr)
calcExpr2 [] acc _ = (acc, [])

calcOp2 :: String -> Int -> Int -> Char -> (Int, String)
calcOp2 expr x acc op = case op of
  '+' -> calcExpr2 expr (acc + x) '('
  '*' -> calcExpr2 expr (acc * x) '*'
  _ -> calcExpr2 expr x '('

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents