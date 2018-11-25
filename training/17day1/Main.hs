module Main where

import Test.HUnit
import Data.Char (isDigit, digitToInt)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  testSolution 'A' "1122" 3,
  testSolution 'A' "1111" 4,
  testSolution 'A' "1234" 0,
  testSolution 'A' "91212129" 9,
  testSolution 'B' "1212" 6,
  testSolution 'B' "1221" 0,
  testSolution 'B' "123425" 4,
  testSolution 'B' "123123" 12,
  testSolution 'B' "12131415" 4
  ]

-- Solving function for each puzzle part
solve :: Char -> String -> Int
solve 'A' = sum . filterPairs . makePairs . parse
solve 'B' = sum . filterPairs . makePairsB . parse

-- Parse the input by filtering out all non-numbers and converting them to int
parse :: String -> [Int]
parse = map digitToInt . filter isDigit

-- Create pairs for A by rotating a copy of the list by one and zipping them
-- Effectively moves the first element to the back, and creates a new list of
-- tuples.
makePairs :: [a] -> [(a,a)]
makePairs [] = []
makePairs (x:xs) = zip (x:xs) (xs ++ [x])

-- Create pairs for B by rotating through half the list and zipping them
-- Shifts the entire list by half around, and creates a list of tuples combining
-- both lists.
makePairsB :: [a] -> [(a,a)]
makePairsB xs = zip xs ((drop n xs) ++ (take n xs))
  where n = (length xs) `div` 2

-- Filter out all pairs that are not equal and reduce to list of Int by picking
-- only the first of two identical elements of the tuple
--   "uncurry (==)" turns the == operator into a function that we can filter with
--   "map fst" picks the first element of each tuple
filterPairs :: Eq a => [(a,a)] -> [a]
filterPairs = map fst . filter (uncurry (==))

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
testSolution :: Char -> String -> Int -> Test
testSolution c x y = TestCase (assertEqual ([c] ++ " " ++ x) (solve c x) y)

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  print $ solve 'A' $ input
  print $ solve 'B' $ input

