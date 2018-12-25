module Main where

import Test.HUnit
import Debug.Trace
import qualified Data.List.Split as Split

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "x=495, y=2..7\n\
            \y=7, x=495..501\n\
            \x=501, y=3..7\n\
            \x=498, y=2..4\n\
            \x=506, y=1..2\n\
            \x=498, y=10..13\n\
            \x=504, y=10..13\n\
            \y=13, x=498..504"

tests = TestList [
  addTest solveA testInput 57
  ]

data Cave = Cave {
    caveSpring :: [(Int, Int)],
    caveWalls :: [(Int, Int)],
    caveWater :: [(Int, Int)],
    caveFlow :: [(Int, Int)]
  } deriving (Show)

parse :: String -> Cave
parse input = foldl (plotWalls) (Cave [(500,0)] [] [] []) . lines $ input
  where
    plotWalls cave line 
      | axis == "x" = foldl (addWall) cave $ [(c0,y) | y <- [c1min..c1max]]
      | axis == "y" = foldl (addWall) cave $ [(x,c0) | x <- [c1min..c1max]]
      | otherwise = cave
      where
        parts = Split.splitOneOf "=,."  $ line
        axis = parts!!0
        c0 = read (parts!!1) :: Int
        c1min = read (parts!!3) :: Int
        c1max = read (parts!!5) :: Int

step :: Cave -> Cave
step = spawnFlow . updateFlow

addFlow :: Cave -> (Int, Int) -> Cave
addFlow cave xy
  | containsCoord xy flow = cave { caveFlow = xy : flow}
  | otherwise = cave
  where
    flow = caveFlow cave

addWall :: Cave -> (Int, Int) -> Cave
addWall cave xy
  | containsCoord xy walls = cave { caveWalls = xy : walls}
  | otherwise = cave
  where
    walls = caveWalls cave

spawnFlow :: Cave -> Cave
spawnFlow cave = foldl (addFlow) cave $ caveSpring cave

updateFlow :: Cave -> Cave
updateFlow cave = foldl (traceFlow) cave (caveFlow cave)

tileType :: Cave -> (Int, Int) -> Char
tileType cave xy
  | xy `elem` (caveWater cave) = '~'
  | xy `elem` (caveWalls cave) = '#'
  | xy `elem` (caveFlow cave) = '|'
  | otherwise = '.'

-- Trace a flow in +y direction until it hits something, returns the length and
-- what blocked it. Fills up as far as it can
traceFlow :: Cave -> (Int, Int) -> Cave
traceFlow cave (x,y)
  | tt == '.' = addFlow cave (x,y+1)
  | otherwise = cave
    where
      tt = tileType cave (x,y + 1)

solveA :: String -> Int
solveA _ = 0

containsCoord :: (Int, Int) -> [(Int, Int)] -> Bool
containsCoord xy = null . filter (\xy' -> xy == xy')

replaceAt :: [a] -> Int -> a -> [a]
replaceAt s idx t = take idx s ++ [t] ++ drop (idx+1) s

replaceAt2 :: [[a]] -> Int -> Int -> a -> [[a]]
replaceAt2 s y x t = replaceAt s y (replaceAt (s!!y) x t)

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
  -- Would be nice to do this automatically. for now i just resolved it by hand
  -- putStrLn "Detecting opcodes:"
  -- findOpcodes $ inputa
  putStrLn "Solution for A:"
  -- print . solveA $ input
  -- putStrLn "Solution for B:"
  -- print . solveB $ inputb
  print . head . drop 3 . iterate (step) . parse $ testInput
