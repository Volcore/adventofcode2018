module Main where

import Test.HUnit
import Data.List.Split
import Data.List
import Data.Int

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  testSolution solveA "abcdef" 0,
  testSolution solveA "bababc" 1,
  testSolution solveA "ababab" 0,
  testSolution solveA "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab" 12,
  testSolution solveB "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz" "fgij"
  ]

-- Solving function for part A
solveA :: String -> Int
solveA input = (*)
            (countDuplicates 2 (parse input))
            (countDuplicates 3 (parse input))

-- Parse the input
parse :: String -> [String]
parse = words

-- Counts the number of n-duplicate-containing-strings in a sequence of strings
countDuplicates :: Int -> [String] -> Int
countDuplicates len = sum . map (containsDuplicate len)

-- Returns 0 if it contains no duplicates of length len,
-- 1 if it contains a duplicate of length len
containsDuplicate :: Int -> String -> Int
containsDuplicate len = min 1 -- limit number returned to 1 or 0
      . length -- compute the number of groups
      . filter (\x -> (length x == len)) -- filter out any groups of wrong len
      . group -- create groups of letters like [['aa'], ['b']]
      . sort -- sort all letters

-- Solving function for part B
solveB :: String -> String
-- solveB input = maybe "error" (findMatches . parse input)
solveB input = case match of
    Just x -> x
    Nothing -> "error"
    where match = findMatch . parse $ input

-- Find recursively find a match between the first list element and all
-- consecutive elements
findMatch :: [String] -> Maybe String
findMatch [] = Nothing
findMatch (x:xs) = case match' x xs of
    Just y  -> Just y
    Nothing -> findMatch xs
    -- match' checks if x matches with any elements of the input list
    where match' x [] = Nothing
          match' x (y:ys) = case isSimilar x y of
            Just z -> Just z
            Nothing -> match' x ys

-- Compares two strings prefix and postfix, and will return pre + post if they
-- only differ by one letter
isSimilar :: String -> String -> Maybe String
isSimilar xs ys = if (length prefix) + (length postfix) == (length xs) - 1
    then Just (prefix ++ postfix)
    else Nothing
  where prefix = findPrefix xs ys
        postfix = findPostfix xs ys
  
-- finds the common postfix of two strings
findPostfix :: String -> String -> String
findPostfix xs ys = reverse (findPrefix (reverse xs) (reverse ys))

-- finds the common prefix of two strings
findPrefix :: String -> String -> String
findPrefix [] ys = []
findPrefix xs [] = []
findPrefix (x:xs) (y:ys) = if x ==y then [x] ++ findPrefix xs ys else []

--------------------------------------------------------------------------------
-- Day-agnostic part. Is the same every day of AoC
--------------------------------------------------------------------------------

-- The testing function. Will run the solve function for challenge A or B on the
-- given input and check the output.
testSolution :: (Show a, Eq a) => (String -> a) -> String -> a -> Test
testSolution f x y = TestCase (assertEqual x y (f x))

-- The main function. Runs the tests, loads the input and runs the solutions
main :: IO ()
main = do
  tt <- runTestTT tests
  input <- readFile "input.txt"
  putStrLn "Solution for A:"
  print . solveA $ input
  putStrLn "Solution for B:"
  print . solveB $ input
  -- Additional tests
  -- print . findPrefix "aabbcc" $ "aabdcc"
  -- print . findPostfix "aabbcc" $ "aabdcc"
  -- print . isSimilar "aabbcc" $ "aabdcc"
  -- print . isSimilar "aabbcc" $ "adkj23"
  -- print . findMatch $ ["aab", "bcd", "bed"]