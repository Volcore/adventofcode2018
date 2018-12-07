module Main where

import Test.HUnit
import qualified Data.List.Split as Split
import qualified Data.List as List

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"

tests = TestList [
  addTest solveA testInput 17,
  addTest (solveB 32) testInput 16
  ]

-- Parse the input
parse :: String -> [(Int, Int)]
parse = map ((\[x, y] -> (read x, read y)) . Split.splitOn ",") . lines

-- Compute the bounds of the list
bounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
bounds l = ((minimum x, maximum x), (minimum y, maximum y))
  where x = fst . unzip $ l
        y = snd . unzip $ l

-- Finds the nearest element, or -1 if it's ambiguous
nearest :: (Int, Int) -> [(Int, Int)] -> Int
nearest (x, y) xs 
    | length minList == 1 = minIdx
    | otherwise = -1
  where
    distList = map (\(x', y') -> (abs (x-x')) + (abs (y-y'))) $ xs
    min = minimum distList
    minList = filter (== min) distList
    minIdx = head $ filter ((== min) . (distList !!)) [0..]

-- Returns the largest non-infinite regions size
getLargest :: [[Int]] -> Int
getLargest xs =
    -- get the largest one
      fst . last . List.sortOn fst
    -- count number of items of each, and create tuples with (count, id)
    . map (\x -> (length x, head x)) . List.group . List.sort
    -- Remove blacklisted items
    . filter (`notElem` blacklist)
    -- flatten list of lists into single list
    . concat $ xs
  where blacklist = (\x -> head x
                        ++ last x
                        ++ head (List.transpose x)
                        ++ last (List.transpose x)) xs

-- Build a map with all grid cells
buildGrid :: ((Int, Int) -> [(Int, Int)] -> Int) -> [(Int, Int)] -> [[Int]]
buildGrid f xs = [[f (x, y) xs | x <- [fst.fst$bnd .. snd.fst$bnd]] 
                | y <- [fst.snd$bnd .. snd.snd$bnd]]
  where bnd = bounds xs

-- Solving function for part A
solveA :: String -> Int
solveA = getLargest . buildGrid (nearest) . parse

-- Finds the nearest element, or -1 if it's ambiguous
maxDist :: Int -> (Int, Int) -> [(Int, Int)] -> Int
maxDist limit (x, y) xs 
    | dist < limit = 1
    | otherwise = 0
  where
    dist = sum . map (\(x', y') -> (abs (x-x')) + (abs (y-y'))) $ xs
  
-- Solving function for part A
solveB :: Int -> String -> Int
solveB limit = length . filter (== 1) . concat . buildGrid (maxDist limit) . parse

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
  print . (solveB 10000) $ input