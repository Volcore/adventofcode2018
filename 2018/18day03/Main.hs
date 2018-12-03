module Main where

import Test.HUnit
import qualified Data.List.Split as Split
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  addTest solveA "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" 4
  ]

-- Parse the input
parse :: String -> [[Int]]
      -- Convert to int
parse = map (map read)
      -- Split into interesting chunks (x, y, w, h) and drop index
      . map (drop 1 . Split.splitOneOf "@:,x")
      -- Split string into lines
      . lines

-- Solving function for part A
solveA :: String -> Int
solveA = snd . blitList . parse

-- Takes a list of rectangles and returns the populated map and the number of
-- collisions
blitList :: [[Int]] -> (Map (Int, Int) Int, Int)
blitList = foldl (blitIntoMap) (Map.empty, 0)

-- Adds a rectangle to the map and returns the map and the number of
-- unique collisions
blitIntoMap :: (Map (Int, Int) Int, Int) -> [Int] -> (Map (Int, Int) Int, Int)
blitIntoMap (m, count) = foldl (blit) (m, count)
                       . coordinatesForRect
    where blit (m, count) coord = case Map.lookup coord m of
                -- If this coordinate is not in the map yet, add it
                Nothing -> (Map.insert coord 1 m, count)
                -- If this coordinate is already in the map once increment its
                -- counter and increment the collision counter
                Just 1 -> (Map.insert coord 2 m, count + 1)
                -- Otherwise just increment the count, but not the collision
                -- counter
                Just x -> (Map.insert coord (x+1) m, count)

-- Generates a list of all coordinates inside a rect
coordinatesForRect :: [Int] -> [(Int, Int)]
coordinatesForRect [x, y, w, h] = [(a, b) | a <- [x..x+w-1], b <- [y..y+h-1]]
coordinatesForRect _ = []

-- Counts the overlaps on a rectangle in the map
countFromMap :: (Map (Int, Int) Int) -> [(Int, Int)] -> Int
countFromMap m = foldl (count) 0
  where count x coord = case Map.lookup coord m of
            -- This shouldn't happen, but don't increment count if not in map
            Nothing -> x
            -- Only in map once means don't increment count
            Just 1 -> x
            -- More than once in map so increment count
            _ -> x + 1

-- Solving function for part B
solveB :: String -> Int
solveB input = count (zip [1..] list) . fst . blitList $ list
  where list = parse input
        -- recursively search for the first rectangle without overlap
        count ((idx, rect):xs) m = case countFromMap m coords of
          -- if no overlap, we've found it
          0 -> idx
          -- otherwise recurse
          _ -> count xs m
          where coords = coordinatesForRect rect

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
