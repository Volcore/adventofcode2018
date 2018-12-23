module Main where

import Test.HUnit
import Debug.Trace
import qualified Data.List.Split as Split

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "pos=<0,0,0>, r=4\n\
            \pos=<1,0,0>, r=1\n\
            \pos=<4,0,0>, r=3\n\
            \pos=<0,2,0>, r=1\n\
            \pos=<0,5,0>, r=3\n\
            \pos=<0,0,3>, r=1\n\
            \pos=<1,1,1>, r=1\n\
            \pos=<1,1,2>, r=1\n\
            \pos=<1,3,1>, r=1"

tests = TestList [
    addTest solveA testInput 7
  ]

data Bot = Bot Int Int Int Int deriving (Show)

botRange :: Bot -> Int
botRange (Bot _ _ _ r) = r

botDistance :: [Bot] -> Int -> Int -> Int
botDistance bots i j = abs(xi-xj) + abs(yi-yj) + abs(zi-zj)
  where
    (Bot xi yi zi _) = bots!!i
    (Bot xj yj zj _) = bots!!j

parse :: String -> [Bot]
parse = map (parseLine) . lines
  where
    parseLine l = (Bot (pi 2) (pi 3) (pi 4) (pi 7))
      where
        parsed = Split.splitOneOf "<,=>" $ l
        pi idx = read (parsed!!idx)

countNeighbors :: [Bot] -> Int -> Int
countNeighbors bots idx = foldl (count) 0 [0..length bots-1]
  where
    count s jdx
      | botDistance bots idx jdx <= botRange (bots!!idx) = s+1
      | otherwise = s

findMaxSignal :: [Bot] -> Int
findMaxSignal bots = foldl (find) 0 $ [0..length bots-1]
  where
    find i j
      | botRange (bots!!j) > botRange (bots!!i) = j
      | otherwise = i

solveA :: String -> Int
solveA = solve . parse
  where
    solve bots = countNeighbors bots . findMaxSignal $ bots

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
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solveA $ input
  putStrLn "Solution for B:"
  -- print . solveB $ input

