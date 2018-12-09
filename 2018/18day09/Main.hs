module Main where

import Test.HUnit
import Data.Map (Map)
import qualified Data.Sequence as Seq
-- import qualified Data.List as List
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------


tests = TestList [
  addTest solveA (9, 25) 32,
  addTest solveA (10, 1618) 8317,
  addTest solveA (13, 7999) 146373,
  addTest solveA (17, 1104) 2764,
  addTest solveA (21, 6111) 54718,
  addTest solveA (30, 5807) 37305
  ]

-- Solving function for part A
solveA :: (Int, Int) -> Int
solveA (np, nm) = solve 0 (Seq.replicate np 0) (Seq.singleton 0) [1..nm]
  where 
    replace s x n = Seq.update n x s
    solve idx scores marbles [] = maximum scores
    solve idx scores marbles (next:ms)
      -- | trace (show next ++ " " ++ show scores ++ " " ++ show marbles ++ " " ++ show idx) False = undefined
      | (next `mod` 23) == 0 = solve newIdx23 scores23 marbles23 ms
      | otherwise = solve newIdx scores marbles' ms
        where 
          newIdx = ((idx + 1) `mod` (length marbles) + 1)
          playerIdx = (next `mod` np)
          marbles' = Seq.insertAt newIdx next marbles
          newIdx23 = ((idx + length marbles - 7) `mod` (length marbles))
          marbles23 = Seq.deleteAt newIdx23 marbles
          newScore23 = (marbles `Seq.index` newIdx23) + next
          scores23 = Seq.adjust' (newScore23 +) playerIdx scores

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
addTest :: (Show a, Eq a, Show b) => (b -> a) -> b -> a -> Test
addTest f x y = TestCase (assertEqual (show x) y (f x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  putStrLn "Solution for A:"
  print . solveA $ (439, 71307)
  putStrLn "Solution for B:"
  print . solveA $ (439, 7130700)