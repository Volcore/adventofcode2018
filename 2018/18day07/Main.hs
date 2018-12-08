module Main where

import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "Step C must be finished before step A can begin.\n\
            \Step C must be finished before step F can begin.\n\
            \Step A must be finished before step B can begin.\n\
            \Step A must be finished before step D can begin.\n\
            \Step B must be finished before step E can begin.\n\
            \Step D must be finished before step E can begin.\n\
            \Step F must be finished before step E can begin."

tests = TestList [
  addTest solveA testInput "CABDFE",
  addTest (solveB 0 2) testInput 15
  ]

-- Parse the input
parse :: String -> [(Char, Char)]
parse = map (\x -> (x!!1!!0, x!!7!!0)) . map words . lines

-- Build a graph structure
graph :: [(Char, Char)] -> Map Char String
graph = build Map.empty
  where build m [] = m
        build m ((x,y):xs) = build (Map.insertWith (++) y [x]
                             . Map.insertWith (++) x [] $ m) xs

-- finds the path from the graph structure for solution A
findPath :: Map Char String -> String
findPath = find ""
  where find s m
          | Map.null m = s
          | otherwise = find (s ++ [next]) m'
            where next = fst . head . filter (\(x, y) -> length y == 0) . Map.toList $ m
                  m' = Map.delete next
                       . Map.map (\ys -> List.delete next ys) $ m

-- Solving function for part A
solveA :: String -> String
solveA = findPath . graph . parse

-- finds the path from the graph structure for solution A
simulate :: Int -> Int -> Map Char String -> Int
simulate delay numw = step (0, replicate numw (0, ' '))
  where step (t, w) m
          | Map.null m = t
          | otherwise = trace (show t ++ show w) step (t+nextt, w') m'
          where nextt = 0 -- minimum . fst . unzip . w
                m' = Map.empty
                w' = trace (show w) w
          -- where idlew
          --         | 
          --       w' = w
          --       m' = m

-- Solving function for part B
solveB :: Int -> Int -> String -> Int
solveB delay workers = simulate delay workers . graph . parse

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
  -- print . solveA $ input
  putStrLn "Solution for B:"
  -- print . (solveB 60 5) $ input
  print . (solveB 0 2) $ testInput