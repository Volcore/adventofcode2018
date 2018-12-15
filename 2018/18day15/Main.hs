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
  addTest solveA testInput1 27730,
  addTest solveA testInput2 36334,
  addTest solveA testInput3 39514,
  addTest solveA testInput4 27755,
  addTest solveA testInput5 28944,
  addTest solveA testInput6 18740,

  addTest solveB testInput1 4988,
  addTest solveB testInput3 31284,
  addTest solveB testInput4 3478,
  addTest solveB testInput5 6474,
  addTest solveB testInput6 1140
  ]

data Unit = Unit {
    unitY :: Int,
    unitX :: Int,
    unitType :: Char,
    unitHealth :: Int
  } deriving (Show, Eq, Ord)

data Game = Game {
    gameUnits :: [Unit],
    gameMap :: [[Char]],
    gameRounds :: Int,
    gameOver :: Bool
  } deriving (Show, Eq, Ord)

parse :: String -> Game
parse s = (Game units m 0 False)
  where
    rawMap = lines $ s
    mapCoords = concat . map (\(row,y) -> map (\(c,x) -> (c,(x,y)))
              $ zip row [0..]) . zip rawMap $ [0..]
    units = foldl (getUnit) [] $ mapCoords
    getUnit us (c, (x,y))
      | isUnit c = us ++ [(Unit y x c 200)]
      | otherwise = us
    m = map (map (\x -> if (isUnit x) then '.' else x)) $ rawMap
    isUnit x
      | x == '.' || x == '#' = False
      | otherwise = True

hasEnemies :: Game -> Char -> Bool
hasEnemies game t = not . null . filter (\(Unit y x t' h) -> t'==t && h>0)
                  $ gameUnits game

step :: Int -> Game -> Game
step ap (Game us m r over) = foldl (update) (Game sorted m (r+1) over) [0..length us-1]
  where
    sorted = sort us
    update game idx
      | h <= 0 = game
      | not $ hasEnemies game enemyType = game { gameOver = True }
      | otherwise = attack ap idx $ move idx $ game
        where 
          us' = (gameUnits game)
          (Unit y x t h) = us'!!idx
          enemyType = if (t == 'E') then 'G' else 'E'

stepUntilVictory :: Int -> Game -> Game
stepUntilVictory ap game
  | gameOver game = game
  | otherwise = stepUntilVictory ap . step ap $ game

move :: Int -> Game -> Game
move idx game = game'
  where
    us = gameUnits game
    (Unit y x t h) = us!!idx
    ds = computeDistanceField game (y,x)
    -- compute target list
    tlist = sort . filter (\(d,_,_)->d>=0)
           . map (\(y,x)->(ds!!y!!x,y,x)) . targetList idx $ game
    (_,ty,tx) = head $ tlist
    game'
      -- don't move if no targets
      | null $ tlist = game
      -- don't move if target is where i already am
      | not . null . filter (\(_,y',x')->y'==y&&x'==x) $ tlist = game
      -- otherwise move towards target
      | otherwise = game { gameUnits = us' }
        where
          -- compute the distance field twoards the target
          mds = computeDistanceField game (ty,tx)
          mt dy dx = (mds!!(y+dy)!!(x+dx),y+dy,x+dx)
          -- compute the list of potential moves
          mlist = sort . filter (\(d,_,_)->d>=0)
                $ [mt (-1) 0,mt 1 0, mt 0 (-1), mt 0 1]
          -- compute the target
          (_,mty,mtx) = head $ mlist
          -- compute updated unit list
          us' = replaceAt us idx (Unit mty mtx t h)

targetList :: Int -> Game -> [(Int, Int)]
targetList i (Game us m r o) = sort $ foldl (find) [] [0..length us-1]
  where
    (Unit y x t h) = us!!i
    -- Find all units in range
    find ts j 
      -- ignore units of the same type
      | t == (unitType (us!!j)) = ts
      -- ignore dead units
      | (unitHealth (us!!j)) <= 0 = ts
      -- add all remaining units
      | otherwise = ts ++ targetsForUnit (us!!j)
    -- Create a list of target locations for a unit
    targetsForUnit (Unit y' x' _ _) = filter (validCoord)
                                    $ [(y',x'-1),(y',x'+1),(y'-1,x'),(y'+1,x')]
    -- Check if a coordinate is valid
    validCoord (y',x')
      -- | trace (show i ++ " " ++ show y' ++ " " ++ show x') False = undefined
      -- current position is always valid
      | x'==x && y'==y = True
      -- walls are not valid
      | m!!y'!!x' == '#' = False
      -- blocked by another alive unit means invalid
      | not . null $ filter (\(Unit y'' x'' _ h'')->x'==x''&&y'==y''&&h''>0) us = False
      -- otherwise it's valid
      | otherwise = True

canWalk :: Game -> (Int, Int) -> Bool
canWalk (Game us m r o) (y,x)
  | m!!y!!x == '#' = False
  | null $ filter (\(Unit y' x' _ h')->h'>0&&x'==x&&y'==y) us = True
  | otherwise = False

computeDistanceField :: Game -> (Int, Int) -> [[Int]]
computeDistanceField game (y,x) = final
  where
    m = gameMap game
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
          | not (canWalk game (y'',x'')) = (xs'', ds'')
          | ds''!!y''!!x'' == -1 || ds''!!y''!!x'' > d+1 = ((d+1,y'',x''): xs'', replaceAt2 ds'' y'' x'' (d+1))
          | otherwise = (xs'', ds'')

reachable :: [[Int]] -> (Int,Int) -> Bool
reachable ds (y,x) = ds!!y!!x > -1

attack :: Int -> Int -> Game -> Game
attack ap i game = game { gameUnits = us' }
  where
    us = gameUnits game
    (Unit _ _ t _) = us!!i
    ap' = if (t=='E') then ap else 3
    alist = attackList i game
    (_,j) = head $ alist
    (Unit y' x' t' h') = us!!j
    us'
      | null $ alist = us 
      | otherwise = replaceAt us j (Unit y' x' t' (h'-ap'))

attackList :: Int -> Game -> [(Int, Int)]
attackList i (Game us m r o) = sort $ foldl (find) [] [0..length us-1]
  where
    (Unit y x t h) = us!!i
    -- Find all enemy units in range
    find ts j
      -- no attacks if type of the target is the same as the attacker
      | t == t' = ts
      -- no attack against dead units
      | h' <= 0 = ts
      | abs (y' - y) <= 1 && x' == x = ts ++ [(h',j)]
      | abs (x' - x) <= 1 && y' == y = ts ++ [(h',j)]
      | otherwise = ts
      where
        (Unit y' x' t' h') = us!!j

score :: Game -> Int
score (Game us m r o) = (r-1) * foldl (score') 0 us
  where
    score' s (Unit y x t h)
      | h < 0 = s
      | otherwise = s + h

solveA :: String -> Int
solveA = score . stepUntilVictory 3 . parse 

solveB :: String -> Int
solveB input = find [3..]
  where
    initial = parse $ input
    countElves game = length
                    . filter (\(Unit _ _ t h) -> t == 'E' && h > 0)
                    . gameUnits $ game
    find (ap:aps)
      | trace ("Trying ap "++show ap) False = undefined
      | countElves sim == countElves initial = score $ sim 
      | otherwise = find aps
        where
          sim = stepUntilVictory ap $ initial
    

plot :: Game -> IO()
plot (Game us m r o) = do
  putStrLn result
  where
    result = foldl (add) "" [0..length m-1]
    add s y = s ++ addUnitRow y (m!!y) ++ "   " ++ addUnits y ++ "\n"
    addUnitRow y row = foldl (addur y) row $ us
    addur y' row (Unit y x t h)
      | h <= 0 = row
      | y' == y = replaceAt row x t
      | otherwise = row
    addUnits y = intercalate ", " . foldl (addu y) [] $ us
    addu y' ul (Unit y x t h)
      | h <= 0 = ul
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
  print . solveA $ input
  putStrLn "Solution for B:"
  print . solveB $ input
  -- let game = parse $ testInput2
  -- plot $ game
  -- plot ((iterate step $ game) !! 1)
  -- plot ((iterate step $ game) !! 2)
  -- plot ((iterate step $ game) !! 3)
  -- plot ((iterate step $ game) !! 4)
  -- plot ((iterate step $ game) !! 32)
  -- plot ((iterate step $ game) !! 33)
  -- plot ((iterate step $ game) !! 34)
  -- plot ((iterate step $ game) !! 35)
  -- plot ((iterate step $ game) !! 36)
  -- plot ((iterate step $ game) !! 37)
  -- plot ((iterate step $ game) !! 38)
  -- plot ((iterate step $ game) !! 23)
  -- plot ((iterate step $ game) !! 24)
  -- plot ((iterate step $ game) !! 38)

-- B 42224 too high
-- B 38922 too low
-- B 41972 was the right one (using the C solution from reddit). All unit
--    tests are passing, and no idea why it's not working.