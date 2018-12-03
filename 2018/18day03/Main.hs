module Main where

import Test.HUnit
import qualified Data.List.Split as Split
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  testSolution solveA "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" 4
  ]

-- Solving function for part A
solveA :: String -> Int
solveA = length
        . parse

blitList :: [[Int]] -> Int
blitList list = fst . foldl (blit) (Map.empty, 0) list
    where blit (m, count) rect = (m, count + (head rect))
-- blit :: (Map.Map k a, Int) -> [Int] -> (Map.Map k a, Int)
-- blit (m,count) rect = (m, count + (head rect))

-- Parse the input
parse :: String -> [[Int]]
      -- Convert to int
parse = map (map read)
      -- Split into interesting chunks (x, y, w, h) and drop index
      . map (drop 1 . Split.splitOneOf "@:,x")
      -- Split string into lines
      . lines

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
testSolution :: (Show a, Eq a) => (String -> a) -> String -> a -> Test
testSolution f x y = TestCase (assertEqual x y (f x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solveA $ input
  -- putStrLn "Solution for B:"
  -- print . solveB $ input
  -- print . parse $ input
  print . blitList . parse $ "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"
