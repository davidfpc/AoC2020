{-# LANGUAGE BlockArguments #-}

module Day1
  ( day1,
  )
where
    
day1 :: IO ()
day1 = do
  -- Read file
  expenses <- readInput "C:\\Users\\Cardoso\\Documents\\adventOfCode\\adventOfCode\\src\\inputFiles\\day1.txt"
  print (part1 expenses)
  print (part2 expenses)


-- Notice the IO [Int] -> this is needed because of the error handling?????
readInput :: FilePath -> IO [Int]
readInput file = do
  -- Read File (IO String, so when doing "<-" whe are executing the IO action, so contents will be a String and not an IO String)
  contents <- readFile file
  -- Convert the content to Int Array
  let numericLines = map (read :: String -> Int) . lines $ contents
  -- putStrLn "Numbers:"
  -- print numericLines
  return numericLines

condition :: (Int,Int) -> Bool
condition tuple = uncurry (+) tuple == 2020

get1st (a,_,_) = a
get2nd (_,a,_) = a
get3rd (_,_,a) = a

condition2 :: (Int,Int,Int) -> Bool
condition2 tuple = do
  get1st tuple + get2nd tuple+ get3rd tuple == 2020

multiply :: (Int,Int,Int) -> Int
multiply tuple = do
  get1st tuple * get2nd tuple * get3rd tuple

part1 :: [Int] -> Int
part1 expenses = do 
    -- make tupples from the list
  let permutations = [ (x, y) | x <- expenses, y <- expenses]
  -- filter the elements that sum up to 2020
  let matchedElements = filter condition permutations
  -- multiply the elements of the tupple
  let results = map (uncurry (*)) matchedElements
  -- ignore that we have two results, I have no idea how to fix that in haskell
  head results

part2 :: [Int] -> Int
part2 expenses = do 
    -- make tupples from the list
  let permutations = [ (x, y, z) | x <- expenses, y <- expenses, z <- expenses]
  -- filter the elements that sum up to 2020
  let matchedElements = filter condition2 permutations
  -- multiply the elements of the tupple
  let results = map multiply matchedElements
  -- ignore that we have three results, I have no idea how to fix that in haskell
  head results