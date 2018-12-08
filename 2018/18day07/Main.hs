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
simulate delay numw = step (-1) (replicate numw (' ', 0))
  where
    step t w m
        -- | trace ("step " ++ show t ++ ": " ++ show w ++ "  " ++ show m) False = undefined
        | Map.null m = t + (maximum . snd . unzip $ w)
        | otherwise =
              -- this could be optimized by finding the largest timestep we can
              -- do (minimum worker time), but it already works so it's fine
          let deltat = 1
              -- subtract the timesteps from each worker, but keep a minimum 0
              ts = map (\(x, t) -> (x, max 0 (t - deltat))) $ w
              -- Split workers into two groups, one ready (time = 0) and one
              -- busy (time > 0)
              (ready, busy) = List.partition ((== 0) . snd) $ ts
              -- find all workers that have completed a task that now needs to
              -- be removed from the dependency graph
              rl = filter (/= ' ') . fst . unzip $ ready
              -- Update the dependency graph, removing all completed tasks
              readyM = foldl (\x y -> Map.map (\ys -> List.delete y ys) x) m rl
              -- Run the assignment helper function on the updated list of
              -- workers. this just passes in already busy workers, and only
              -- runs the assignment function for every worker that is idle.
              -- This was inspired by the reddit aoc2018 megathread.
              (w', m') = foldl assign (busy, readyM) $ [1..length ready]
          in step (t + deltat) w' m'
          where
            -- How long is a given task?
            tasklen c = (fromEnum c) - (fromEnum 'A') + delay + 1
            assign (cw, cm) _ 
              -- | trace("assign " ++ show cw ++ " " ++ show cm) False = undefined
              -- If nothing available, add worked back to front of list
              | Map.null $ Map.filter (== []) cm = ((' ', 0) : cw ,cm)
              -- Something is available for this worked
              | otherwise =
                -- find the next task by filtering the list for empty
                -- dependencies and get the smallest/first element
                let nextTask = fst . Map.findMin . Map.filter (== []) $ cm 
                -- Add new worker to the front of the queue
                in ((nextTask, tasklen nextTask) : cw , Map.delete nextTask cm)

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
  print . solveA $ input
  putStrLn "Solution for B:"
  print . (solveB 60 5) $ input
