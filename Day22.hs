module Day22 (main) where

import Data.Set (Set, empty)
import Debug.Trace (trace, traceShow)

main :: IO ()
main = do
  -- Read file
  input <- readInput "./inputFiles/day22.txt"
  let _ : player1Deck = map read (takeWhile (not . null) input)
  let _ : player2Deck = map read (drop (length player1Deck + 2) input)
  print $ part1 player1Deck player2Deck
  print $ part2 player1Deck player2Deck empty

part1 :: [Int] -> [Int] -> Int
part1 (card1 : player1Cards) (card2 : player2Cards) =
  if card1 > card2
    then part1 (player1Cards ++ [card1, card2]) player2Cards
    else part1 player1Cards (player2Cards ++ [card2, card1])
part1 [] player2Cards = calcScore player2Cards
part1 player1Cards [] = calcScore player1Cards

part2 :: [Int] -> [Int] -> Set (Int, Int) -> Int
part2 (card1 : player1Cards) (card2 : player2Cards) _ =
  if card1 > card2
    then part2 (player1Cards ++ [card1, card2]) player2Cards empty
    else part2 player1Cards (player2Cards ++ [card2, card1]) empty
part2 [] player2Cards _ = calcScore player2Cards
part2 player1Cards [] _ = calcScore player1Cards

evaluateCards :: [Int] -> [Int] -> Set (Int, Int) -> ([Int], [Int], Set (Int, Int))
evaluateCards (card1 : player1Cards) (card2 : player2Cards) cardsPlayed = ([], [], empty)

calcScore :: [Int] -> Int
calcScore cards =
  let value = reverse [1 .. length cards]
   in sum $ zipWith (*) cards value

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents
