module Main where

import Test.HUnit
import Data.List (sortBy)
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  addTest (powerLevel 8) (3, 5) 4,
  addTest (powerLevel 57) (122, 79) (-5),
  addTest (powerLevel 39) (217, 196) 0,
  addTest (powerLevel 71) (101, 153) 4,
  addTest solveB 18 (90,269,16),
  addTest solveB 42 (232,251,12),
  addTest solveA 18 (33,45,3),
  addTest solveA 42 (21,61,3)
  ]

fixCoords :: (Int, Int, Int) -> (Int, Int, Int)
fixCoords (x,y,s) = (x+1,y+1,s)

-- Solving function for part A
solveA :: Int -> (Int, Int, Int)
solveA sn = fixCoords . findMaxForKernelSize 3 n . createSAT n . createGrid sn $ n
  where n = 300

powerLevel :: Int -> (Int, Int) -> Int
powerLevel sn (x, y) = (thirdDigit (((rackId * y) + sn) * rackId)) - 5
  where
    rackId = x + 10
    thirdDigit z = (z `quot` 100) - ((z `quot` 1000) * 10)

totalPower :: [[Int]] -> (Int, Int, Int) -> Int
totalPower s (x,y,n) = sat (yn, xn) + sat (y, x) - sat (y, xn) - sat (yn, x)
  where
    xn = x+n
    yn = y+n
    sat (u,v)
      | u <= 0 || v <= 0 = 0
      | otherwise = s !!(u-1) !! (v-1)

findMaxForKernelSize :: Int -> Int -> [[Int]] -> (Int, Int, Int)
findMaxForKernelSize k n sat = snd . head . sortBy (flip compare) $ list
  where
    range = [0..(n-k)]
    list = [(totalPower sat (x,y,k), (x,y,k)) | x <- range, y <- range]

solveB :: Int -> (Int, Int, Int)
solveB sn = 
  let 
    n = 300
    sat = createSAT n $ createGrid sn n
    -- f x n = trace (show n ++ show x) head . sortBy (flip compare) $ grid n ++ [x]
    f x k = head . sortBy (flip compare) $ grid k ++ [x]
    range k = [0..(n-k)]
    grid k = [(totalPower sat (x,y,k), (x,y,k)) | x <- range k, y <- range k]
  in fixCoords . snd . foldl (f) (-9999,(0,0,0)) $ [1..n]

-- Create a grid of dimension n
createGrid :: Int -> Int -> [[Int]]
createGrid sn n = [[powerLevel sn (x, y) | x <- [1..n]] | y <- [1..n]]

-- Create a summed-area table
createSAT :: Int -> [[Int]] -> [[Int]]
createSAT n grid = foldl (nextRow) [] [0..n-1]
  where
    nextRow sat y = sat ++ [foldl (nextElem) [] [0..n-1]]
      where
        nextElem row x = row ++ [grid!!y!!x
                                + lookup (x-1,y)
                                + lookup (x,y-1)
                                - lookup (x-1,y-1)]
          where
            lookup (u,v)
              | u < 0 || v < 0 = 0
              | v == y = row!!u
              | otherwise = sat!!v!!u

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
  putStrLn "Solution for B:"
  print . solveB $ 5153
