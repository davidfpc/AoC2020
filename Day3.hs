module Day3 (main) where

main :: IO ()
main = do
  -- Read file
  treeMap <- readInput "./inputFiles/day3.txt"
  print $ part1 treeMap
  print $ part2 treeMap

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

secondElement (_, a, _) = a

secondElement2 (_, a, _, _) = a

-- find how many trees are on the slope 3
part1 :: [String] -> Int
part1 treeMap = secondElement $ foldl countTree (0, 0, 3) treeMap

-- find how many trees are on slope 1, 3, 5, 6, and 1 (2 vertical) and multiply the result
part2 :: [String] -> Int
part2 treeMap =
  secondElement (foldl countTree (0, 0, 1) treeMap)
    * secondElement (foldl countTree (0, 0, 3) treeMap)
    * secondElement (foldl countTree (0, 0, 5) treeMap)
    * secondElement (foldl countTree (0, 0, 7) treeMap)
    * secondElement2 (foldl countTreeSkip (0, 0, 1, True) treeMap)

-- given the current index, count, slope and tree map line, check if there is a tree. If there is increment the count and index, otherwise just increment the index
countTree :: (Int, Int, Int) -> String -> (Int, Int, Int)
countTree (index, currentCount, slope) line = if (line !! mod (index * slope) 31) == '#' then (index + 1, currentCount + 1, slope) else (index + 1, currentCount, slope)

-- for vertical skips, just check if we are on a skipable row.
-- If skipable row, just skip. Otherwise check if there is a tree. If there, is increment the count and index, otherwise just increment the index
countTreeSkip :: (Int, Int, Int, Bool) -> String -> (Int, Int, Int, Bool)
countTreeSkip (index, currentCount, slope, skip) line =
  if skip
    then
      if (line !! mod (index * slope) 31) == '#'
        then (index + 1, currentCount + 1, slope, not skip)
        else (index + 1, currentCount, slope, not skip)
    else (index, currentCount, slope, not skip)