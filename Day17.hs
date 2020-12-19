module Day17 (main) where

import Data.Set (Set, delete, empty, filter, foldl, fromList, insert, map, member, size)
import Debug.Trace (traceShow)

type Position = (Int, Int, Int)

type Position4D = (Int, Int, Int, Int)

main :: IO ()
main = do
  -- Read file
  dimension <- readInput "./inputFiles/day17.txt"
  let input = transformInput dimension 0 empty
  let input4D = transformInput4D dimension 0 empty
  print $ part1 input 0
  print $ part2 input4D 0

-- part 1, how many cubes are active after 6 generations
part1 :: Set Position -> Int -> Int
part1 dimension generation = case generation of
  6 -> size dimension
  _ -> part1 (calcGeneration dimension) (generation + 1)

-- part 1, how many cubes are active after 6 generations
part2 :: Set Position4D -> Int -> Int
part2 dimension generation = case generation of
  6 -> size dimension
  _ -> part2 (calcGeneration4D dimension) (generation + 1)

calcGeneration :: Set Position -> Set Position
calcGeneration dimension = Data.Set.foldl (calcPosition dimension) empty dimension

calcGeneration4D :: Set Position4D -> Set Position4D
calcGeneration4D dimension = Data.Set.foldl (calcPosition4D dimension) empty dimension

calcPosition :: Set Position -> Set Position -> Position -> Set Position
calcPosition dimension newGen pos =
  let neighbour = getNeighbourPos pos
      activeNeighbour = size $ Data.Set.foldl (\a x -> if member x dimension then insert x a else a) empty neighbour
      updatedNewGen = Data.Set.foldl (\a x -> if calcPosition' dimension a x then insert x a else a) newGen neighbour
   in if activeNeighbour == 2 || activeNeighbour == 3 then insert pos updatedNewGen else updatedNewGen

calcPosition' :: Set Position -> Set Position -> Position -> Bool
calcPosition' dimension newGen pos =
  let neighbour = getNeighbourPos pos
      activeNeighbour = size $ Data.Set.foldl (\a x -> if member x dimension then insert x a else a) empty neighbour
   in activeNeighbour == 3

calcPosition4D :: Set Position4D -> Set Position4D -> Position4D -> Set Position4D
calcPosition4D dimension newGen pos =
  let neighbour = getNeighbourPos4D pos
      activeNeighbour = size $ Data.Set.foldl (\a x -> if member x dimension then insert x a else a) empty neighbour
      updatedNewGen = Data.Set.foldl (\a x -> if calcPosition4D' dimension a x then insert x a else a) newGen neighbour
   in if activeNeighbour == 2 || activeNeighbour == 3 then insert pos updatedNewGen else updatedNewGen

calcPosition4D' :: Set Position4D -> Set Position4D -> Position4D -> Bool
calcPosition4D' dimension newGen pos =
  let neighbour = getNeighbourPos4D pos
      activeNeighbour = size $ Data.Set.foldl (\a x -> if member x dimension then insert x a else a) empty neighbour
   in activeNeighbour == 3

getNeighbourPos :: Position -> Set Position
getNeighbourPos (x, y, z) =
  fromList
    [ (x -1, y -1, z -1),
      (x, y -1, z -1),
      (x + 1, y - 1, z -1),
      (x -1, y, z -1),
      (x, y, z -1),
      (x + 1, y, z -1),
      (x -1, y + 1, z -1),
      (x, y + 1, z -1),
      (x + 1, y + 1, z -1),
      (x -1, y -1, z),
      (x, y -1, z),
      (x + 1, y - 1, z),
      (x -1, y, z),
      (x + 1, y, z),
      (x -1, y + 1, z),
      (x, y + 1, z),
      (x + 1, y + 1, z),
      (x -1, y -1, z + 1),
      (x, y -1, z + 1),
      (x + 1, y - 1, z + 1),
      (x -1, y, z + 1),
      (x, y, z + 1),
      (x + 1, y, z + 1),
      (x -1, y + 1, z + 1),
      (x, y + 1, z + 1),
      (x + 1, y + 1, z + 1)
    ]

getNeighbourPos4D :: Position4D -> Set Position4D
getNeighbourPos4D (x, y, z, w) =
  fromList
    [ (x -1, y -1, z -1, w -1),
      (x, y -1, z -1, w -1),
      (x + 1, y - 1, z -1, w -1),
      (x -1, y, z -1, w -1),
      (x, y, z -1, w -1),
      (x + 1, y, z -1, w -1),
      (x -1, y + 1, z -1, w -1),
      (x, y + 1, z -1, w -1),
      (x + 1, y + 1, z -1, w -1),
      (x -1, y -1, z, w -1),
      (x, y -1, z, w -1),
      (x + 1, y - 1, z, w -1),
      (x -1, y, z, w -1),
      (x, y, z, w -1),
      (x + 1, y, z, w -1),
      (x -1, y + 1, z, w -1),
      (x, y + 1, z, w -1),
      (x + 1, y + 1, z, w -1),
      (x -1, y -1, z + 1, w -1),
      (x, y -1, z + 1, w -1),
      (x + 1, y - 1, z + 1, w -1),
      (x -1, y, z + 1, w -1),
      (x, y, z + 1, w -1),
      (x + 1, y, z + 1, w -1),
      (x -1, y + 1, z + 1, w -1),
      (x, y + 1, z + 1, w -1),
      (x + 1, y + 1, z + 1, w -1),
      (x -1, y -1, z -1, w),
      (x, y -1, z -1, w),
      (x + 1, y - 1, z -1, w),
      (x -1, y, z -1, w),
      (x, y, z -1, w),
      (x + 1, y, z -1, w),
      (x -1, y + 1, z -1, w),
      (x, y + 1, z -1, w),
      (x + 1, y + 1, z -1, w),
      (x -1, y -1, z, w),
      (x, y -1, z, w),
      (x + 1, y - 1, z, w),
      (x -1, y, z, w),
      (x + 1, y, z, w),
      (x -1, y + 1, z, w),
      (x, y + 1, z, w),
      (x + 1, y + 1, z, w),
      (x -1, y -1, z + 1, w),
      (x, y -1, z + 1, w),
      (x + 1, y - 1, z + 1, w),
      (x -1, y, z + 1, w),
      (x, y, z + 1, w),
      (x + 1, y, z + 1, w),
      (x -1, y + 1, z + 1, w),
      (x, y + 1, z + 1, w),
      (x + 1, y + 1, z + 1, w),
      (x -1, y -1, z -1, w + 1),
      (x, y -1, z -1, w + 1),
      (x + 1, y - 1, z -1, w + 1),
      (x -1, y, z -1, w + 1),
      (x, y, z -1, w + 1),
      (x + 1, y, z -1, w + 1),
      (x -1, y + 1, z -1, w + 1),
      (x, y + 1, z -1, w + 1),
      (x + 1, y + 1, z -1, w + 1),
      (x -1, y -1, z, w + 1),
      (x, y -1, z, w + 1),
      (x + 1, y - 1, z, w + 1),
      (x -1, y, z, w + 1),
      (x, y, z, w + 1),
      (x + 1, y, z, w + 1),
      (x -1, y + 1, z, w + 1),
      (x, y + 1, z, w + 1),
      (x + 1, y + 1, z, w + 1),
      (x -1, y -1, z + 1, w + 1),
      (x, y -1, z + 1, w + 1),
      (x + 1, y - 1, z + 1, w + 1),
      (x -1, y, z + 1, w + 1),
      (x, y, z + 1, w + 1),
      (x + 1, y, z + 1, w + 1),
      (x -1, y + 1, z + 1, w + 1),
      (x, y + 1, z + 1, w + 1),
      (x + 1, y + 1, z + 1, w + 1)
    ]

readInput :: FilePath -> IO [String]
readInput file = do
  contents <- readFile file
  return $ lines contents

transformInput :: [String] -> Int -> Set Position -> Set Position
transformInput (row : remaining) y set =
  let index = [0 .. (length row - 1)]
      updatedSet = Prelude.foldl (\a x -> if (row !! x) == '#' then insert (x, y, 0) a else a) set index
   in transformInput remaining (y -1) updatedSet
transformInput [] _ set = set

transformInput4D :: [String] -> Int -> Set Position4D -> Set Position4D
transformInput4D (row : remaining) y set =
  let index = [0 .. (length row - 1)]
      updatedSet = Prelude.foldl (\a x -> if (row !! x) == '#' then insert (x, y, 0, 0) a else a) set index
   in transformInput4D remaining (y -1) updatedSet
transformInput4D [] _ set = set