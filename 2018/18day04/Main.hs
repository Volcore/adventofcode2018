module Main where

import Test.HUnit
import Text.Printf (printf)
import Data.List (sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List.Split as Split

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "[1518-11-01 00:00] Guard #10 begins shift\n\
            \[1518-11-01 00:05] falls asleep\n\
            \[1518-11-01 00:30] falls asleep\n\
            \[1518-11-02 00:50] wakes up\n\
            \[1518-11-03 00:05] Guard #10 begins shift\n\
            \[1518-11-03 00:24] falls asleep\n\
            \[1518-11-01 00:55] wakes up\n\
            \[1518-11-04 00:46] wakes up\n\
            \[1518-11-03 00:29] wakes up\n\
            \[1518-11-04 00:02] Guard #99 begins shift\n\
            \[1518-11-01 00:25] wakes up\n\
            \[1518-11-01 23:58] Guard #99 begins shift\n\
            \[1518-11-02 00:40] falls asleep\n\
            \[1518-11-04 00:36] falls asleep\n\
            \[1518-11-05 00:03] Guard #99 begins shift\n\
            \[1518-11-05 00:45] falls asleep\n\
            \[1518-11-05 00:55] wakes up"

tests = TestList [
  addTest solveA testInput 240,
  addTest solveB testInput 4455
  ]

data LogEntryType = Start | Sleep | Wake
  deriving (Show, Eq, Ord)

data LogEntry = LogEntry {
    year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int,
    entryType :: LogEntryType,
    guardId :: Int
  }
  deriving (Show, Ord, Eq)

instance Read LogEntry where
  readsPrec _ value = [(LogEntry (read y)
                                 (read m)
                                 (read d)
                                 (read h)
                                 (read min)
                                 getType getId, [])]
    -- read the log entry by splitting it and then working on each entries
    -- does a heuristic detection of the message by looking at the first word
    where (_:y:m:d:h:min:_:t:ti:_) = Split.splitOneOf "[]-: " value
          getType = case t of
            "Guard" -> Start
            "falls" -> Sleep
            otherwise -> Wake
          getId = case t of
            "Guard" -> read . drop 1 $ ti
            otherwise -> 0

-- Parse the input
parse :: String -> [LogEntry]
parse = sort . map read . lines

data GuardInfo = GuardInfo {
    sleep :: Int,
    sleepstart :: Int,
    sleeplog :: [Int],
    gid :: Int
  }
  deriving (Show, Ord, Eq)

-- analyses the log and creates a guard info map with details on each guard
analyse :: [LogEntry] -> Map Int GuardInfo
analyse = fst . foldl (analyse') (Map.empty, 0)
  where analyse' (m, cgid) le = case entryType le of
          -- For the start case create a new entry in the map for this guard
          -- if it does not exist, otherwise just remember that this guard
          -- is on duty now in cgid
          Start -> case Map.member newgid m of          
                      False -> (Map.insert newgid newgi m, newgid)
                      True -> (m, newgid)             
                     where newgid = guardId le
                           newgi = (GuardInfo 0 0 (replicate 60 0) newgid)
          -- Guard goes to sleep. remember the time when
          Sleep -> (Map.adjust (update) cgid m, cgid)
            where update x = x { sleepstart = (minute le) }
          -- Guard wakes up from sleep, measure the delta and update the sleep
          -- log
          Wake -> (Map.adjust (update) cgid m, cgid)
            where now = minute le
                  update x = x {
                      sleep = (sleep x) + delta,
                      sleeplog = logSleep start now (sleeplog x)
                      }
                    where start = (sleepstart x)
                          delta = now - start

-- logs the current sleep interval into the sleep log of a guard
logSleep :: Int -> Int -> [Int] -> [Int]
logSleep start end = map (\(x, y) -> if x >= start && x < end then y + 1 else y)
                   . zip [0..59]

-- takes the map, finds the guard with the most sleep, and returns it
findSleeper :: Map Int GuardInfo -> GuardInfo
findSleeper = head . sortBy (flip compare) . snd . unzip . Map.toList

-- Solving function for part A
solveA :: String -> Int
solveA = out . findSleeper . analyse . parse
  where maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]
        out gi = (maxIndex (sleeplog gi)) * (gid gi)

-- Searches all guard infos for the guard that sleeps the most on a given minute
-- and returns guardid, minute and how often he sleeps on that minute
findSleeperB :: Map Int GuardInfo -> (Int, Int, Int)
findSleeperB = foldl (find) (0, 0, 0) . Map.toList
  where find (maxgid, maxminute, maxcount) (cgid, gi) = if (fst cur) > maxcount
          -- if this guard sleeps more than the current max, replace
          then (cgid, (snd cur), (fst cur))
          -- otherwise keep using the current max
          else (maxgid, maxminute, maxcount)
            -- to compute the current sleep, zip up minutes and counts, then
            -- sort by count descending and return the first element
            where cur = head
                      . sortBy (flip compare)
                      . zip (sleeplog gi) $ [0..59]

-- Solving function for part B
solveB :: String -> Int
solveB = (\(x, y, z) -> x * y) . findSleeperB . analyse . parse

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
