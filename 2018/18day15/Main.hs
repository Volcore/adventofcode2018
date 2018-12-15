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
step (Game us m) = foldl (update) (Game (sort us) m) [0..length us-1]
  where
    update game idx = attack idx . move idx $ game

move :: Int -> Game -> Game
move idx game = game

targetList :: Int -> Game -> [(Int, Int)]
targetList i (Game us m) = sort $ foldl (find) [] [0..length us-1]
  where
    (Unit x y t h) = us!!i
    -- Find all units in range
    find ts j 
      | t == (unitType (us!!j)) = ts
      | (unitHealth (us!!j)) <= 0 = ts
      | otherwise = ts ++ targetsForUnit (us!!j)
    -- Create a list of target locations for a unit
    targetsForUnit (Unit y' x' _ _) = filter (validCoord)
                                    $ [(y',x'-1),(y',x'+1),(y'-1,x'),(y'+1,x')]
    -- Check if a coordinate is valid
    validCoord (y',x')
      | x'==x && y'==y = True
      | m!!y'!!x' == '#' = False
      | null $ filter (\(Unit y'' x'' _ _)->x'==x''&&y'==y'') us = True
      | otherwise = False

canWalk :: Game -> (Int, Int) -> Bool
canWalk (Game us m) (y,x)
  | m!!y!!x == '#' = False
  | null $ filter (\(Unit y' x' _ _)->x'==x&&y'==y) us = True
  | otherwise = False

computeDistanceField :: Game -> (Int, Int) -> [[Int]]
computeDistanceField (Game us m) (y,x) = final
  where
    empty = replicate (length m) (replicate (length (m!!0)) (-1))
    initial = replaceAt2 empty y x 0
    final = update initial [(0,y,x)]
    update ds [] = ds
    update ds ((_,y',x'):xs) = update ds' (sort xs')
      where
        d = ds!!y'!!x'
        (xs', ds') = check (y'+1,x')
                   . check (y'-1,x')
                   . check (y',x'+1)
                   . check (y',x'-1) $ (xs, ds)
        check (y'',x'') (xs'', ds'')
          | not (canWalk (Game us m) (y'',x'')) = (xs'', ds'')
          | ds''!!y''!!x'' == -1 || ds''!!y''!!x'' > d+1 = ((d+1,y'',x''): xs'', replaceAt2 ds'' y'' x'' (d+1))
          | otherwise = (xs'', ds'')

reachable :: [[Int]] -> (Int,Int) -> Bool
reachable ds (y,x) = ds!!y!!x > -1

attack :: Int -> Game -> Game
attack idx game = game
  -- where
  --   inputUs = sort us
  --   us' = foldl (up) [] [0..length us-1]
  --   up newus idx = newus ++ [move (inputUs!!idx)]
  --     where
  --       others = newus ++ take (idx+1) inputUs
  --       move (Unit y x t h) = range . targets $ t 
  --       targets t = filter (\(Unit _ _ t' _) -> t /= t') others
  --       range ts = filter ()
  --                . map (\(Unit x y _ _)->[(x-1,y),(x+1,y),(x,y-1),(x,y+1)]) $ ts

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
  putStrLn "Solution for A:"
  -- print . solveA $ 306281
  putStrLn "Solution for B:"
  let game = parse $ testInput5
  plot $ game
  -- plot ((iterate step . parse $ testInput1) !! 1)
  -- plot ((iterate step . parse $ testInput1) !! 2)
  -- plot ((iterate step . parse $ testInput1) !! 3)
  print . targetList 0 $ game
  print . filter (reachable (computeDistanceField game (1,2))) . targetList 0 $ game
  print . computeDistanceField game $ (1,2)

