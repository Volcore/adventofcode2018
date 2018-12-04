module Main where

import Test.HUnit
import Data.Time.Format
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  addTest solveA "[1518-11-01 00:00] Guard #10 begins shift\n\
                 \[1518-11-01 00:05] falls asleep\n\
                 \[1518-11-01 00:25] wakes up\n\
                 \[1518-11-01 00:30] falls asleep\n\
                 \[1518-11-01 00:55] wakes up\n\
                 \[1518-11-01 23:58] Guard #99 begins shift\n\
                 \[1518-11-02 00:40] falls asleep\n\
                 \[1518-11-02 00:50] wakes up\n\
                 \[1518-11-03 00:05] Guard #10 begins shift\n\
                 \[1518-11-03 00:24] falls asleep\n\
                 \[1518-11-03 00:29] wakes up\n\
                 \[1518-11-04 00:02] Guard #99 begins shift\n\
                 \[1518-11-04 00:36] falls asleep\n\
                 \[1518-11-04 00:46] wakes up\n\
                 \[1518-11-05 00:03] Guard #99 begins shift\n\
                 \[1518-11-05 00:45] falls asleep\n\
                 \[1518-11-05 00:55] wakes up" 240
  ]

data LogEntryType = Start | Sleep | Wake
  deriving (Show, Eq)
data LogEntry = LogEntry {
    date :: Int,
    minute :: Int,
    entryType :: LogEntryType,
    guardId :: Int
  }

instance Show LogEntry where
  show le = printf "%d %d %s %d"
              (date le) (minute le) (show (entryType le)) (guardId le)

instance Read LogEntry where
  readsPrec _ value = [(LogEntry 0 0 Start 0, [])]

-- Parse the input
parse :: String -> [LogEntry]
parse = map read . lines


-- Solving function for part A
solveA :: String -> Int
solveA = length . parse

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
  -- print . solveA $ input
  -- putStrLn "Solution for B:"
  -- print . solveB $ input
  print . parse $ input
