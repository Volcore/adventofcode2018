module Main where

import Test.HUnit
import Data.List.Split as Split
import Debug.Trace

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "position=< 9,  1> velocity=< 0,  2>\n\
            \position=< 7,  0> velocity=<-1,  0>\n\
            \position=< 3, -2> velocity=<-1,  1>\n\
            \position=< 6, 10> velocity=<-2, -1>\n\
            \position=< 2, -4> velocity=< 2,  2>\n\
            \position=<-6, 10> velocity=< 2, -2>\n\
            \position=< 1,  8> velocity=< 1, -1>\n\
            \position=< 1,  7> velocity=< 1,  0>\n\
            \position=<-3, 11> velocity=< 1, -2>\n\
            \position=< 7,  6> velocity=<-1, -1>\n\
            \position=<-2,  3> velocity=< 1,  0>\n\
            \position=<-4,  3> velocity=< 2,  0>\n\
            \position=<10, -3> velocity=<-1,  1>\n\
            \position=< 5, 11> velocity=< 1, -2>\n\
            \position=< 4,  7> velocity=< 0, -1>\n\
            \position=< 8, -2> velocity=< 0,  1>\n\
            \position=<15,  0> velocity=<-2,  0>\n\
            \position=< 1,  6> velocity=< 1,  0>\n\
            \position=< 8,  9> velocity=< 0, -1>\n\
            \position=< 3,  3> velocity=<-1,  1>\n\
            \position=< 0,  5> velocity=< 0, -1>\n\
            \position=<-2,  2> velocity=< 2,  0>\n\
            \position=< 5, -2> velocity=< 1,  2>\n\
            \position=< 1,  4> velocity=< 2,  1>\n\
            \position=<-2,  7> velocity=< 2, -2>\n\
            \position=< 3,  6> velocity=<-1, -1>\n\
            \position=< 5,  0> velocity=< 1,  0>\n\
            \position=<-6,  0> velocity=< 2,  0>\n\
            \position=< 5,  9> velocity=< 1, -2>\n\
            \position=<14,  7> velocity=<-2,  0>\n\
            \position=<-3,  6> velocity=< 2, -1>"

parse :: String -> [[Int]]
parse = map (map read)
      . map (\x -> [x!!1, x!!2, x!!4, x!!5])
      . map (Split.splitOneOf "<>,")
      . lines

simulate :: Int -> [[Int]] -> [[Int]]
simulate n = foldr (step') []
  where
    step' xs out = [xs!!0 + n * xs!!2, xs!!1 + n * xs!!3, xs!!2, xs!!3] : out

bounds :: [[Int]] -> [Int]
bounds state = [minimum xs, maximum xs, minimum ys, maximum ys]
  where
    xs = map (head) $ state
    ys = map (head . drop 1) $ state

atPosition :: Int -> Int -> [[Int]] -> Bool
atPosition x y state = (length
                       . filter (\(x':y':_) -> x' == x && y' == y) $ state) > 0

plot :: [[Int]] -> IO ()
plot state = do
  let b = bounds $ state
  putStrLn . (++) "Bounds: " . show $ b
  putStr result
  where
      b = bounds $ state
      contains x y
        | atPosition x y state = '*'
        | otherwise = ' '
      row y = [contains x y | x <- [b!!0..b!!1]]
      result = concat [row y ++ "\n" | y <- [b!!2..b!!3]]

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
  -- Solution for A & B
  let steps = 10619
  print $ steps
  let state = simulate steps . parse $ input
  plot $ state
