module Main where

import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

tests = TestList [
  addTest solveA testInput 138,
  addTest solveB testInput 66
  ]

data LNode = LNode [LNode] [Int]
  deriving (Show)

-- Sum over all metadata of the node
sumNodes :: LNode -> Int
sumNodes (LNode children meta) = sum meta + (sum . map (sumNodes) $ children)

-- compute the value of the node
nodeValue :: LNode -> Int
nodeValue (LNode children meta)
  --   Leaving this here as a reminder how to debug for future me
  --   | trace ("nodeValue: " ++ show children ++ show meta ) False = undefined
  -- If the node has no children just return the metadata sums
  | length children == 0 = sum meta
  -- Else sum over all children referenced by the metadata
  | otherwise = sum . map (value) $ meta
    where value x
            -- Check the children id. if it's valid, recurse into it
            | x <= length children = nodeValue $ (children !! (x-1))
            -- Else the value for this entry is zero
            | otherwise = 0

-- Parse the input
parse :: String -> [Int]
parse = map read . words

-- reads the next node from the list of integers and applies it to the end of
-- the input node array (siblings)
readTree :: ([LNode], [Int]) -> ([LNode], [Int])
readTree (siblings, numnodes:nummeta:xs) = 
      -- iteratively apply readTree on itself to read the next numnodes nodes
      -- each iteration parses one more node from the list, so select the n-th
      -- iteration and return. childs has a list of all sub nodes, and rem the
      -- remaining input list (plus the metadata for this node)
  let (childs, rem) = iterate readTree ([], xs) !! numnodes
      -- read the metadata for this node from the remainder
      metadata = take nummeta rem
      -- the remainder after removing all subnodes and the metadata
      rest = drop nummeta rem
  -- append the new node to the siblings, and pass through the rest of the list
  in (siblings ++ [LNode childs metadata], rest)

-- Solving function for part A
solveA :: String -> Int
solveA input = sumNodes . head . fst . readTree $ ([], parse input)

-- Solving function for part B
solveB :: String -> Int
solveB input = nodeValue . head . fst . readTree $ ([], parse input)

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