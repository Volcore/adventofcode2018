module Main where

import Test.HUnit
import Data.List.Split
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  testSolution 'A' "+1\n+1\n+1" 3,
  testSolution 'A' "+1\n+1\n-2" 0,
  testSolution 'A' "-1\n-2\n-3\n" (-6),
  testSolution 'B' "+1\n-1" 0,
  testSolution 'B' "+3\n+3\n+4\n-2\n-4" 10,
  testSolution 'B' "-6\n+3\n+8\n+5\n-6" 5,
  testSolution 'B' "+7\n+7\n-2\n-7\n-4" 14
  ]

-- Solving function for each puzzle part
solve :: Char -> String -> Int
solve 'A' = sum . parse
solve 'B' = findDuplicate . cycle . parse

-- Parse the input
parse :: String -> [Int]
parse = map read -- convert to int
      . map (filter (\x -> (x /= '+'))) -- Remove plus
      . lines

-- find duplicates in an infinite list
findDuplicate :: [Int] -> Int
findDuplicate = findDuplicate' 0 Set.empty -- kick off the search
  where findDuplicate' acc set (x:xs) = if Set.member acc set
        -- if the current accumulated number is known, we've found it
        then acc 
        -- else recurse into this, computing a new accumulated value and 
        -- adding the current value to the set
        else findDuplicate' (acc+x) (Set.insert acc set) xs

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
testSolution :: Char -> String -> Int -> Test
testSolution c x y = TestCase (assertEqual ([c] ++ " " ++ x) y (solve c x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solve 'A' $ input
  putStrLn "Solution for B:"
  print . solve 'B' $ input

