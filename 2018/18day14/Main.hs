module Main where

import Test.HUnit
import Debug.Trace
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  addTest solveA 9 "5158916779",
  addTest solveA 5 "0124515891",
  addTest solveA 18 "9251071085",
  addTest solveA 2018 "5941429882"
  -- addTest solveB testInput2 (6,4)
  ]
  
digits :: Int -> [Int]
digits = map (read . (:[])) . show

run :: ([Int], [Int]) -> ([Int], [Int])
run (rs,es) = 
  let
    r i = rs!!(es!!i)
    newRs = rs ++ digits (r 0 + r 1)
    lenRs = length newRs
    newEs = [(r 0 + es!!0 + 1) `mod` lenRs, (r 1 + es!!1 + 1) `mod` lenRs]
  in (newRs, newEs)

runN :: Int -> ([Int], [Int]) -> ([Int], [Int])
runN n (rs, es)
  | length rs >= n + 10 = (rs, es)
  | otherwise = trace (show $ length rs) runN n $ run (rs, es)

solveA :: Int -> String
solveA n = map (\x -> toEnum (fromEnum '0' + x)) . take 10 . drop n . fst . runN n $ ([3, 7], [0, 1])

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
  print . solveA $ 306281
  putStrLn "Solution for B:"
  -- print . solveB $ input
  -- print . run $ ([3, 7], [0, 1])
  -- print . run . run $ ([3, 7], [0, 1])
  -- print . run . run . run $ ([3, 7], [0, 1])
  -- print . runN 9 $ ([3, 7], [0, 1])