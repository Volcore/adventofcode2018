module Main where

import Test.HUnit

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "dabAcCaCBAcCcaDA"

tests = TestList [
  addTest solveA testInput 10
  -- addTest solveB testInput 4455
  ]


-- Parse the input
parse :: String -> String
parse = head. lines


-- Solving function for part A
solveA :: String -> Int
solveA = length . parse

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
addTest :: (Show a, Eq a) => (String -> a) -> String -> a -> Test
addTest f x y = TestCase (assertEqual x y (f x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solveA $ input
  -- putStrLn "Solution for B:"
  -- print . solveB $ input
