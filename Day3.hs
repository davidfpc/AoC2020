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

secondElement (_,a,_) = a
secondElement2 (_,a,_,_) = a

part1 :: [String] -> Int
part1 treeMap = secondElement $ foldl countTree (0, 0, 3) treeMap

part2 :: [String] -> Int
part2 treeMap = (secondElement $ foldl countTree (0, 0, 1) treeMap)
                * (secondElement $ foldl countTree (0, 0, 3) treeMap)
                * (secondElement $ foldl countTree (0, 0, 5) treeMap)
                * (secondElement $ foldl countTree (0, 0, 7) treeMap)
                * (secondElement2 $ foldl countTreeSkip (0, 0, 1, True) treeMap)

countTree :: (Int, Int, Int) -> String -> (Int, Int, Int)
countTree (index, currentCount, slope) line = if (line !! mod (index * slope) 31) == '#' then (index + 1, currentCount + 1, slope) else (index + 1, currentCount, slope)

countTreeSkip :: (Int, Int, Int, Bool) -> String -> (Int, Int, Int, Bool)
countTreeSkip (index, currentCount, slope, skip) line = if skip then
  if (line !! mod (index * slope) 31) == '#' then
    (index + 1, currentCount + 1, slope, not skip)
    else (index + 1, currentCount, slope, not skip)
  else (index, currentCount, slope, not skip)