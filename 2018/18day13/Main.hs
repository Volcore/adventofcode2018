module Main where

import Test.HUnit
import Debug.Trace
import Data.List
import Data.Ord (Down, comparing)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "/->-\\         \n\
            \|   |  /----\\ \n\
            \| /-+--+-\\  | \n\
            \| | |  | v  | \n\
            \\\-+-/  \\-+--/ \n\
            \  \\------/   "

testInput2 = "/>-<\\  \n\
             \|   |  \n\
             \| /<+-\\\n\
             \| | | v\n\
             \\\>+</ |\n\
             \  |   ^\n\
             \  \\<->/"

tests = TestList [
  addTest solveA testInput (7,3),
  addTest solveB testInput2 (6,4)
  ]

data Cart = Cart {
  cartX :: Int,
  cartY :: Int,
  cartDir :: Int,
  cartTurn :: Int
  }
  deriving (Show, Eq)

instance Ord Cart where
  compare = comparing cartY

type MapState = ([String], [Cart])

parse :: String -> MapState
parse input = (lines . map (repl) $ input, findCarts)
  where
    repl c
      | c == '>' || c == '<' = '-'
      | c == 'v' || c == '^' = '|'
      | otherwise = c
    findCarts = snd . foldl (findCart) ((0,0),[]) $ input
    findCart ((x,y), carts) c
      | c == '\n' = ((0, y+1), carts)
      | c == '>' = ((x+1, y), carts ++ [(Cart x y 0 0)])
      | c == 'v' = ((x+1, y), carts ++ [(Cart x y 1 0)])
      | c == '<' = ((x+1, y), carts ++ [(Cart x y 2 0)])
      | c == '^' = ((x+1, y), carts ++ [(Cart x y 3 0)])
      | otherwise = ((x+1, y), carts)

stepN :: Int -> MapState -> MapState
stepN 0 ms = ms
stepN i ms = stepN (i-1) (removeCrashed $ step ms)

stepUntil :: MapState -> MapState
stepUntil (map, carts)
  | null . filter (\(Cart _ _ d _) -> d == 4) $ carts = stepUntil . step $ (map, carts)
  | otherwise = (map, carts)

stepUntilLast :: MapState -> MapState
stepUntilLast (map, carts)
  | (length . filter (\(Cart _ _ d _) -> d /= 4) $ carts) > 1 = stepUntilLast . removeCrashed . step $ (map, carts)
  | otherwise = (map, carts)

removeCrashed :: MapState -> MapState
removeCrashed (map, carts) = (map, filter (\(Cart _ _ d _) -> d /= 4) carts)

step :: MapState -> MapState
step (map, carts) = (map, updateCarts)
  where
    updateCarts = foldl (update) (sort $ carts) [0..(length carts) - 1]
    update carts idx = collides idx
                        (take idx carts
                        ++ [turn . move $ carts!!idx]
                        ++ drop (idx+1) carts)
    collides idx carts
      | (length . filter (\(Cart x' y' _ _) -> x' == x && y' == y) $ carts) <= 1 = carts
      | otherwise = trace (show x ++ "/" ++ show y) [ if (x' == x && y' == y)
                      then (Cart x' y' 4 t')
                      else (Cart x' y' d' t')
                      | (Cart x' y' d' t') <- carts]
      where
        x = cartX (carts!!idx)
        y = cartY (carts!!idx)
    move (Cart x y d t)
      | d == 0 = (Cart (x+1) y d t)
      | d == 1 = (Cart x (y+1) d t)
      | d == 2 = (Cart (x-1) y d t)
      | d == 3 = (Cart x (y-1) d t)
      | otherwise = (Cart x y d t)
    turnValue t = [3, 0, 1] !! t
    turn (Cart x y d t)
      | d == 4 = (Cart x y d t)
      | map!!y!!x == '+' = (Cart x y (mod (d + turnValue t) 4) (mod (t+1) 3))
      | map!!y!!x == '\\' && d == 0 = (Cart x y 1 t)
      | map!!y!!x == '\\' && d == 1 = (Cart x y 0 t)
      | map!!y!!x == '\\' && d == 2 = (Cart x y 3 t)
      | map!!y!!x == '\\' && d == 3 = (Cart x y 2 t)
      | map!!y!!x == '/' && d == 0 = (Cart x y 3 t)
      | map!!y!!x == '/' && d == 1 = (Cart x y 2 t)
      | map!!y!!x == '/' && d == 2 = (Cart x y 1 t)
      | map!!y!!x == '/' && d == 3 = (Cart x y 0 t)      
      | otherwise = (Cart x y d t)

-- Solving function for part A
solveA :: String -> (Int, Int)
solveA = (\(Cart x y _ _) -> (x, y)) 
        . head
        . filter (\(Cart _ _ d _) -> d == 4)
        . snd
        . stepUntil
        . parse

solveB :: String -> (Int, Int)
solveB = (\(Cart x y _ _) -> (x, y)) 
        . head
        . filter (\(Cart _ _ d _) -> d /= 4)
        . snd
        . stepUntilLast
        . parse

plot :: MapState -> IO ()
plot (map, carts) = do
  let m = foldl (put) map $ carts
  putStrLn . intercalate "\n" $ m
  print $ carts
  where
    put map' (Cart x y d t) = take y map' ++ [placeCart (map' !! y) x d] ++ drop (y+1) map'
    placeCart row x d = take x row ++ [cart d] ++ drop (x+1) row
    cart d = ">v<^X"!!d

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
  -- putStrLn $ testInput
  -- print . parse $ testInput
  -- plot . parse $ testInput
  -- plot . removeCrashed . step . removeCrashed . step . removeCrashed . step . parse $ testInput2
  -- plot . stepN 1 . parse $ testInput
  -- plot . stepN 2 . parse $ testInput
  -- plot . stepN 3 . parse $ testInput
  -- plot . stepN 4 . parse $ testInput
  -- plot . stepN 5 . parse $ testInput
  -- plot . stepN 6 . parse $ testInput
  -- plot . stepN 7 . parse $ testInput
  -- plot . stepN 8 . parse $ testInput
  -- plot . stepN 9 . parse $ testInput
  -- plot . stepN 10 . parse $ testInput
  -- plot . stepN 11 . parse $ testInput
  -- plot . stepN 12 . parse $ testInput
  -- plot . stepN 13 . parse $ testInput
  -- plot . stepN 14 . parse $ testInput
  -- plot . stepN 15 . parse $ testInput
  -- plot . stepN 16 . parse $ testInput
  -- plot . stepN 17 . parse $ testInput
  -- plot . stepN 19 . parse $ testInput
  -- plot . stepN 20 . parse $ testInput
  -- plot . stepN 21 . parse $ testInput
  -- plot . stepN 22 . parse $ testInput
  -- plot . stepN 23 . parse $ testInput
  -- plot . stepN 24 . parse $ testInput
  plot . stepN 322 . parse $ input
  -- plot . stepUntil . parse $ testInput
  -- plot . stepUntilLast . parse $ input
  -- plot . stepN 165 . parse $ input
  -- plot . stepN 1 . parse $ input
