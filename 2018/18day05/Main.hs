module Main where

import Test.HUnit
import Data.Char (toLower, toUpper)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "dabAcCaCBAcCcaDA"

tests = TestList [
  addTest solveA testInput 10,
  addTest solveB testInput 4
  ]

-- Parse the input
parse :: String -> String
parse = head . lines

-- Collapse if they are not identical but their lowercases are
doesCollapse :: Char -> Char -> Bool
doesCollapse x y = x /= y && toLower x == toLower y

-- Collapses a sequence as much as possible and returns the result
collapse :: String -> String
collapse = foldr collapse' ""
  where
    collapse' x (y:xs)
      | doesCollapse x y = xs
    collapse' x xs = [x] ++ xs

-- Solving function for part A
solveA :: String -> Int
solveA = length . collapse . parse

-- Solving function for part B
solveB :: String -> Int
solveB = minimum
        -- Apply the collapse function
        . map (length . collapse)
        -- remove the letter given by the first tuple element from the string
        -- given in the second tuple element
        . map (\(x,ys) -> [ y | y <- ys, not (y `elem` [x, toUpper x])])
        -- create tuples of the input and each letter of the alphabet
        . zip ['a'..'z']
        -- create 26 copies of the input string in a list
        . replicate 26
        . parse

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
  putStrLn "Solution for B:"
  print . solveB $ input