module Main where

import Test.HUnit
import Debug.Trace
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List (isInfixOf)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  addTest solveA 9 "5158916779",
  addTest solveA 5 "0124515891",
  addTest solveA 18 "9251071085",
  addTest solveA 2018 "5941429882",
  addTest solveB [5,1,5,8,9] 9,
  addTest solveB [0,1,2,4,5] 5,
  addTest solveB [9,2,5,1,0] 18,
  addTest solveB [5,9,4,1,4] 2018
  ]

initializer :: (Seq.Seq Int, [Int])
initializer = (Seq.fromList [3, 7], [0, 1])
  
digits :: Int -> [Int]
digits = map (read . (:[])) . show

run :: (Seq.Seq Int, [Int]) -> (Seq.Seq Int, [Int])
run (rs,es) = 
  let
    r i = rs `Seq.index` (es!!i)
    newRs = rs Seq.>< Seq.fromList (digits (r 0 + r 1))
    lenRs = length newRs
    newEs = [(r 0 + es!!0 + 1) `mod` lenRs, (r 1 + es!!1 + 1) `mod` lenRs]
  in (newRs, newEs)

runN :: Int -> (Seq.Seq Int, [Int]) -> (Seq.Seq Int, [Int])
runN n (rs, es)
  | length rs >= n + 10 = (rs, es)
  | otherwise = runN n $ run (rs, es)

solveA :: Int -> String
solveA n = map (\x -> toEnum (fromEnum '0' + x)) . take 10 . drop n . toList . fst . runN n $ initializer

solveB :: [Int] -> Int
solveB s = find s . toList . fst . runN 5000 $ initializer
-- solveB s = fst . foldl (up) (0, initializer) $ [1..]
--   where
--     up (n, (rs, es)) _
--       | trace (show rs) False = undefined
--       | s `isInfixOf` (toList rs) = (n, (rs, es))
--       | otherwise = (n+1, run $ (rs, es))

find :: (Eq a, Show a) => [a] -> [a] -> Int
find (x:xs) [] = -99999999
find xs ys
  -- | trace (show xs ++ " " ++ show ys) False = undefined
  | prefix xs ys = 0
  | otherwise = 1 + find xs (tail ys)

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

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
  -- print . solveA $ 306281
  putStrLn "Solution for B:"
  -- print . find [5,1,5,8,9] . toList . fst . runN 1000 $ initializer
  print . find [3,0,6,2,8,1] . toList . fst . runN 10000000 $ initializer  
  -- print . solveB $ input
  -- print . run $ ([3, 7], [0, 1])
  -- print . run . run $ ([3, 7], [0, 1])
  -- print . run . run . run $ ([3, 7], [0, 1])
  -- print . runN 9 $ ([3, 7], [0, 1])