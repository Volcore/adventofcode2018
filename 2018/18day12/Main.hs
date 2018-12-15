module Main where

import qualified Test.HUnit as HUnit
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "initial state: #..#.#..##......###...###\n\
            \ \n\
            \...## => #\n\
            \..#.. => #\n\
            \.#... => #\n\
            \.#.#. => #\n\
            \.#.## => #\n\
            \.##.. => #\n\
            \.#### => #\n\
            \#.#.# => #\n\
            \#.### => #\n\
            \##.#. => #\n\
            \##.## => #\n\
            \###.. => #\n\
            \###.# => #\n\
            \####. => #"

tests = HUnit.TestList [
  addTest solveA testInput 325
  ]

data Rule = Rule {
  ruleKey :: String,
  ruleValue :: Char
} deriving Show

data State = State {
  stateMap :: String,
  stateRules :: [Rule],
  stateLeft :: Int
} deriving Show

-- Matches a rule pattern to a sequence, and returns the char for it, or '.'
-- if none is found
matchPattern :: [Rule] -> String -> Char
matchPattern [] _ = '.'
matchPattern ((Rule key val):rs) p
  | key == p = val
  | otherwise = matchPattern rs p

-- Compute the score by iterating over all cells and adding the value for the
-- set cells
score :: State -> Int
score (State m _ l) = score' m l 0
  where
    score' [] _ s = s
    score' (x:xs) idx s
      | x == '.' = score' xs (idx+1) s
      | otherwise = score' xs (idx+1) (s+idx)

-- Step through one iteration. Adds a pretty excessive "worst case" amount of
-- padding every time, but that's fine for 20 iterations
step :: State -> State
step state = (State newMap rules newLeft)
  where
    -- 
    newLeft = stateLeft state - 2
    rules = stateRules state
    padMap = "...." ++ stateMap state ++ "...."
    newMap = foldr (up) [] [2..length padMap-3]
    up idx m = (match idx) : m
    match idx = matchPattern rules (take 5 . drop (idx-2) $ padMap)

-- Call the step function n times
stepN :: Int -> State -> State
stepN 0 state = state
stepN n state = stepN (n-1) . step $ state

-- Parse the input
parse :: String -> State
parse input = (State initial rules 0)
  where
    -- Initial is the last word in the first line
    initial = last . words . head . lines $ input
    -- all lines beyond the second line are of a simple format:
    -- word 1 is pattern
    -- word 3 is value
    rules = map ((\(x:_:(y:_):_) -> (Rule x y)) . words) . drop 2 . lines $ input

-- Solving function for part A
solveA :: String -> Int
solveA = score . stepN 20 . parse

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
addTest :: (Show a, Eq a, Show b) => (b -> a) -> b -> a -> HUnit.Test
addTest f x y = HUnit.TestCase (HUnit.assertEqual (show x) y (f x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- HUnit.runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solveA $ input
  -- putStrLn "Solution for B:"
  -- print . solveB $ input
  -- putStrLn ""
  -- print . step . step . parse $ testInput
  -- print . stepN 20 . parse $ testInput
  -- print . map ((\(x:_:(y:_):_) -> (Rule x y)) . words) . drop 2 . lines $ testInput
