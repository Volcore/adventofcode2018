module Main where

import Test.HUnit
import Data.Char (isDigit, digitToInt)
import Data.List.Split

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  testSolution 'A' "5 1 9 5\n7 5 3\n2 4 6 8" 18,
  testSolution 'B' "5 9 2 8\n9 4 7 3\n3 8 6 5" 9
  ]

-- Solving function for each puzzle part
solve :: Char -> String -> Int
solve 'A' = sum . map findMinMaxDiff . parse
solve 'B' = sum . map findEvenDiv . parse

-- Parse the input by filtering out all non-numbers and converting them to int
parse :: String -> [[Int]]
parse = map (map read) -- convert to int
      . map (words) -- split on whitespace
      . filter (\x -> (length x /= 0)) -- filter all empty lines
      . splitOn "\n" -- split lines

-- Find the minimum and maximum of a line and compute the difference
findMinMaxDiff :: [Int] -> Int
findMinMaxDiff xs = (foldr1 max xs) - (foldr1 min xs)

-- Find the evenly divisible pairs in the line and return the quotient
findEvenDiv :: [Int] -> Int
findEvenDiv = head -- Get the first one (supposely only one in each row)
            . map (\(x, y) -> x `quot` y) -- Compute quotient
            . filter (\(x, y) -> x `mod` y == 0) -- Filter out invalid pairs
            . outerProduct -- Compute all possible pairs

-- Computes a pseudo outer product of itself, ignoring identic pairs
-- [a b c] -> [ab ac ba bc ca cb]
outerProduct :: [Int] -> [(Int, Int)]
outerProduct xs = [(x,y) | x <- xs, y <- xs, x /= y]

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
testSolution :: Char -> String -> Int -> Test
testSolution c x y = TestCase (assertEqual ([c] ++ "\n" ++ x) (solve c x) y)

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solve 'A' $ input
  putStrLn "Solution for B:"
  print . solve 'B' $ input

