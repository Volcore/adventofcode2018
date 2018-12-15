module Main where

import Test.HUnit
import Debug.Trace
import Data.List (sort, intercalate)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput1 = "#######\n\
             \#.G...#\n\
             \#...EG#\n\
             \#.#.#G#\n\
             \#..G#E#\n\
             \#.....#\n\
             \#######"

testInput2 = "#######\n\
             \#G..#E#\n\
             \#E#E.E#\n\
             \#G.##.#\n\
             \#...#E#\n\
             \#...E.#\n\
             \#######"

testInput3 = "#######\n\
             \#E..EG#\n\
             \#.#G.E#\n\
             \#E.##E#\n\
             \#G..#.#\n\
             \#..E#.#\n\
             \#######"
       
testInput4 = "#######\n\
             \#E.G#.#\n\
             \#.#G..#\n\
             \#G.#.G#\n\
             \#G..#.#\n\
             \#...E.#\n\
             \#######"
             
testInput5 = "#######\n\
             \#.E...#\n\
             \#.#..G#\n\
             \#.###.#\n\
             \#E#G#G#\n\
             \#...#G#\n\
             \#######"

testInput6 = "#########\n\
             \#G......#\n\
             \#.E.#...#\n\
             \#..##..G#\n\
             \#...##..#\n\
             \#...#...#\n\
             \#.G...G.#\n\
             \#.....G.#\n\
             \#########"

             
tests = TestList [
  -- addTest solveA testInput1 27730,
  -- addTest solveA testInput2 36334,
  -- addTest solveA testInput3 39514,
  -- addTest solveA testInput4 27755,
  -- addTest solveA testInput5 28944,
  -- addTest solveA testInput6 18740,
  ]

data Unit = Unit {
    unitY :: Int,
    unitX :: Int,
    unitType :: Char,
    unitHealth :: Int
  } deriving (Show, Eq, Ord)

data Game = Game {
    gameUnits :: [Unit],
    gameMap :: [[Char]]
  } deriving (Show, Eq, Ord)

parse :: String -> Game
parse s = (Game units m)
  where
    rawMap = lines $ s
    mapCoords = concat . map (\(row,y) -> map (\(c,x) -> (c,(x,y))) $ zip row [0..]) . zip rawMap $ [0..]
    units = foldl (getUnit) [] $ mapCoords
    getUnit us (c, (x,y))
      | isUnit c = us ++ [(Unit y x c 200)]
      | otherwise = us
    m = map (map (\x -> if (isUnit x) then '.' else x)) $ rawMap
    isUnit x
      | x == '.' || x == '#' = False
      | otherwise = True

step :: Game -> Game
step (Game us m) = (Game us' m)
  where
    inputUs = sort us
    us' = foldl (up) [] [0..length us-1]
    up newus idx = newus ++ [move (inputUs!!idx)]
      where
        others = newus ++ take (idx+1) inputUs
        move (Unit y x t h) = range . targets $ t 
        targets t = filter (\(Unit _ _ t' _) -> t /= t') others
        range ts = map (\(Unit x y _ _)->[(x-1,y),(x+1,y),(x,y-1),(x,y+1)]) $ ts

solveA :: String -> Int
solveA s = 0

plot :: Game -> IO()
plot (Game us m) = do
  putStrLn result
  where
    result = foldl (add) "" [0..length m-1]
    add s y = s ++ addUnitRow y (m!!y) ++ "   " ++ addUnits y ++ "\n"
    addUnitRow y row = foldl (addur y) row $ us
    addur y' row (Unit y x t h)
      | y' == y = replaceAt row x t
      | otherwise = row
    addUnits y = intercalate ", " . foldl (addu y) [] $ us
    addu y' ul (Unit y x t h)
      | y' == y = ul ++ [t : "(" ++ show h ++ ")"]
      | otherwise = ul

replaceAt :: String -> Int -> Char -> String
replaceAt s idx t = take idx s ++ [t] ++ drop (idx+1) s

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
  putStrLn "Solution for A:"
  -- print . solveA $ 306281
  putStrLn "Solution for B:"
  plot . parse $ testInput1
  plot ((iterate step . parse $ testInput1) !! 1)
  plot ((iterate step . parse $ testInput1) !! 2)
  plot ((iterate step . parse $ testInput1) !! 3)

  -- let rawMap = lines $ testInput1
  -- let mapCoords = concat . map (\(row,y) -> map (\(c,x) -> (c,(x,y))) $ zip row [0..]) . zip rawMap $ [0..]
  -- print $ mapCoords