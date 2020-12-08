module Day8 (main) where

import Text.Parsec
  ( char,
    digit,
    letter,
    many1,
    parse,
    (<|>),
  )
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  -- Read file
  intructions <- readInput "./inputFiles/day8.txt"
  let parsedIntructions = rights $ map (parse parseInput "") intructions
  print $ firstElement $ part1 parsedIntructions (0, 0, [])
  print $ firstElement2 $ part2 parsedIntructions (0, False, 0, [])

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

-- part 1, iterate the program until finding a loop (assuming that there is a loop and the program doesn't end normally)
part1 :: [(String, Int)] -> (Int, Int, [Int]) -> (Int, Int, [Int])
part1 instructions (accu, pos, visitedPos) =
  if pos `elem` visitedPos
    then (accu, pos, visitedPos)
    else
      let instruction = instructions !! pos
       in case fst instruction of
            "acc" -> part1 instructions (accu + snd instruction, pos + 1, pos : visitedPos)
            "jmp" -> part1 instructions (accu, pos + snd instruction, pos : visitedPos)
            "nop" -> part1 instructions (accu, pos + 1, pos : visitedPos)

-- part 2, iterate the program changing nop or jmp until the program completes (end normally)
part2 :: [(String, Int)] -> (Int, Bool, Int, [Int]) -> (Int, Bool, Int, [Int])
part2 instructions (accu, changed, pos, visitedPos) =
  if pos `elem` visitedPos || pos >= length instructions
    then (accu, changed, pos, visitedPos)
    else
      let instruction = instructions !! pos
       in case fst instruction of
            "acc" -> part2 instructions (accu + snd instruction, changed, pos + 1, pos : visitedPos)
            "jmp" ->
              if changed
                then part2 instructions (accu, changed, pos + snd instruction, pos : visitedPos)
                else let changeResult = part2 instructions (accu, True, pos + 1, pos : visitedPos) in if thirdElement changeResult >= length instructions then changeResult else part2 instructions (accu, changed, pos + snd instruction, pos : visitedPos)
            "nop" ->
              if changed
                then part2 instructions (accu, changed, pos + 1, pos : visitedPos)
                else let changeResult = part2 instructions (accu, True, pos + snd instruction, pos : visitedPos) in if thirdElement changeResult >= length instructions then changeResult else part2 instructions (accu, changed, pos + 1, pos : visitedPos)

parseInput :: Parser (String, Int)
parseInput = do
  operator <- many1 letter
  char ' '
  signal <- char '-' <|> char '+'
  arg <- many1 digit
  return (operator, if signal == '-' then - read arg :: Int else read arg)

firstElement (a, _, _) = a

firstElement2 (a, _, _, _) = a

thirdElement (_, _, a, _) = a

rights (Right x : xs) = x : rights xs
rights (_ : xs) = rights xs
rights [] = []