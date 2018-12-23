module Main where

import Test.HUnit
import Debug.Trace
import Data.List (intercalate, sort, group)
import Data.List.Split (chunksOf)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = ".#.#...|#.\n\
            \.....#|##|\n\
            \.|..|...#.\n\
            \..|#.....#\n\
            \#.#|||#|#|\n\
            \...#.||...\n\
            \.|....|...\n\
            \||...#|.#|\n\
            \|.||||..|.\n\
            \...#.|..|."

tests = TestList [
  addTest solveA testInput 1147
  ]

parse :: String -> [[Char]]
parse = lines 

solveA :: String -> Int
solveA = foldl (*) 1
       . map (length)
       . group
       . sort
       . filter (`elem` "|#")
       . concat
       . head
       . drop 10
       . iterate (step)
       . parse

getNeighbors :: Int -> Int -> [[Char]] -> (Int, Int)
getNeighbors x y m = foldl (count) (0, 0) $ [(x,y)|x<-[x-1..x+1],y<-[y-1..y+1]]
  where
    count (ntrees, nyards) (x',y')
      | x' == x && y' == y = (ntrees, nyards)
      | x' < 0 || x' > (length m)-1 = (ntrees, nyards)
      | y' < 0 || y' > (length m)-1 = (ntrees, nyards)
      | t == '#' = (ntrees, nyards+1)
      | t == '|' = (ntrees+1, nyards)
      | otherwise = (ntrees, nyards)
      where
        t = m!!y'!!x'

step :: [[Char]] -> [[Char]]
step m = foldl (step') m $ [(x,y)|x<-[0..length m-1],y<-[0..length m-1]]
  where
    step' m' (x,y)
      | t == '.' && ntrees >= 3 = replaceAt2 m' y x '|'
      | t == '|' && nyards >= 3 = replaceAt2 m' y x '#'
      | t == '#' && nyards >= 1 && ntrees >=1 = m'
      | t == '#' = replaceAt2 m' y x '.'
      | otherwise = m'
      where
        t = m!!y!!x
        (ntrees, nyards) = getNeighbors x y m

replaceAt :: [a] -> Int -> a -> [a]
replaceAt s idx t = take idx s ++ [t] ++ drop (idx+1) s

replaceAt2 :: [[a]] -> Int -> Int -> a -> [[a]]
replaceAt2 s y x t = replaceAt s y (replaceAt (s!!y) x t)


--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
addTest :: (Show a, Eq a, Show b) => (b -> a) -> b -> a -> Test
addTest f x y = TestCase (assertEqual (show x) y (f x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solveA $ input
  -- putStrLn "Solution for B:"
  -- print . solveB $ inputb
  -- putStrLn . intercalate "\n" . parse $ testInput
  -- print . getNeighbors 6 1 . parse $ testInput
  -- putStrLn . intercalate "\n" . step . parse $ testInput
  -- putStrLn . intercalate "\n" . head . drop 10 . iterate (step) . parse $ testInput
  -- print . group . sort . filter (`elem` "|#") . concat . head . drop 10 . iterate (step) . parse $ testInput
  -- print . foldl (*) 1 . map (length) . group . sort . filter (`elem` "|#") . concat . head . drop 10 . iterate (step) . parse $ testInput
