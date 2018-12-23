module Main where

import Test.HUnit
import Debug.Trace
import Data.List (intercalate, sort, group)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map

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

stepSmart :: Map String Int -> Int -> [[Char]] -> [[Char]]
stepSmart hist count m
  | count == 0 = m
  | Map.member k hist = trace("Found repetition at "
                              ++show count
                              ++" from "
                              ++show lastCount
                              ++ " remain "
                              ++ show remain)
      head . drop remain . iterate (step) $ m
  | otherwise = stepSmart hist' (count-1) (step m)
  where
    k = concat $ m 
    hist' = Map.insert k count hist
    (Just lastCount) = Map.lookup k hist
    remain = count `mod` (lastCount - count)

solveB :: String -> Int
solveB = foldl (*) 1
       . map (length)
       . group
       . sort
       . filter (`elem` "|#")
       . concat
       . stepSmart Map.empty 1000000000
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
  putStrLn "Solution for B:"
  print . solveB $ input
