module Main where

import Test.HUnit
import Data.List (sortBy)
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  addTest solveB 18 (90,269,16),
  addTest solveB 42 (232,251,12),
  addTest (powerLevel 8) (3, 5) 4,
  addTest (powerLevel 57) (122, 79) (-5),
  addTest (powerLevel 39) (217, 196) 0,
  addTest (powerLevel 71) (101, 153) 4,
  addTest solveA 18 (33,45),
  addTest solveA 42 (21,61)
  ]

-- Solving function for part A
solveA :: Int -> (Int, Int)
solveA sn = snd . head . sortBy (flip compare) $ grid
  where
    grid = [(totalPower sn (x, y), (x, y)) | x <- [1..298], y <- [1..298]]

totalPower :: Int -> (Int, Int) -> Int
totalPower sn (x,y) = sum $ mask
  where
    mask = [powerLevel sn (x+x', y+y') | x'<-[0..2] , y'<-[0..2]]

powerLevel :: Int -> (Int, Int) -> Int
powerLevel sn (x, y) = (thirdDigit (((rackId * y) + sn) * rackId)) - 5
  where
    rackId = x + 10
    thirdDigit z = (z `quot` 100) - ((z `quot` 1000) * 10)

totalPowerN :: Int -> (Int, Int, Int) -> Int
totalPowerN sn (x,y,n) = sum $ mask
  where
    mask = [powerLevel sn (x+x', y+y') | x'<-[0..(n-1)] , y'<-[0..(n-1)]]

solveB :: Int -> (Int, Int, Int)
solveB sn = snd . head . sortBy (flip compare) . foldl (f) [] $ [1..300]
  where
    f xs n = trace (show n) [head . sortBy (flip compare) $ grid n] ++ xs
    range n = [1..(301-n)]
    grid n = [(totalPowerN sn (x,y,n), (x,y,n)) | x <- range n, y <- range n]

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
  print . solveA $ 5153
  -- putStrLn "Solution for B:"
  -- print . solveA $ input
  -- print . totalPower 5153 $ (2, 2)
  -- print . snd . head . sortBy (flip compare) $ [(totalPower 5153 (x, y), (x, y)) | x <- [2..299], y <- [2..299]]