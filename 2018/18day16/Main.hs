module Main where

import Test.HUnit
import Debug.Trace
import Data.Bits
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "Before: [3, 2, 1, 1]\n\
            \9 2 1 2\n\
            \After:  [3, 2, 2, 1]\n\
            \"


              
tests = TestList [
  addTest solveA testInput 1
  ]

data TestA = TestA {
    testAInput :: [Int],
    testAOutput :: [Int],
    testAInstruction :: [Int]
  } deriving (Show)

exec :: [Int] -> [Int] -> [Int]
exec reg (op:a:b:c:_) = reg'
  where
    [r0,r1,r2,r3] = reg
    opMap op = [14,7,1,5,9,15,0,11,6,10,8,13,2,3,12,4]!!op
    reg'
      -- addr
      | opMap op == 0 = replaceAt reg c (reg!!a + reg!!b)
      -- addi
      | opMap op == 1 = replaceAt reg c (reg!!a + b)
      -- mulr
      | opMap op == 2 = replaceAt reg c (reg!!a * reg!!b)
      -- muli
      | opMap op == 3 = replaceAt reg c (reg!!a * b)
      -- banr
      | opMap op == 4 = replaceAt reg c (reg!!a .&. reg!!b)
      -- bani
      | opMap op == 5 = replaceAt reg c (reg!!a .&. b)
      -- borr
      | opMap op == 6 = replaceAt reg c (reg!!a .|. reg!!b)
      -- bori
      | opMap op == 7 = replaceAt reg c (reg!!a .|. b)
      -- setr
      | opMap op == 8 = replaceAt reg c (reg!!a)
      -- seti
      | opMap op == 9 = replaceAt reg c (a)
      -- gtir
      | opMap op == 10 = replaceAt reg c (if (a > reg!!b) then 1 else 0)
      -- gtri
      | opMap op == 11 = replaceAt reg c (if (reg!!a > b) then 1 else 0)
      -- gtrr
      | opMap op == 12 = replaceAt reg c (if (reg!!a > reg!!b) then 1 else 0)
      -- eqir
      | opMap op == 13 = replaceAt reg c (if (a == reg!!b) then 1 else 0)
      -- eqri
      | opMap op == 14 = replaceAt reg c (if (reg!!a == b) then 1 else 0)
      -- eqrr
      | opMap op == 15 = replaceAt reg c (if (reg!!a == reg!!b) then 1 else 0)
      | otherwise = trace ("Unknown opcode " ++ show op) reg

stripChars :: String -> String -> String
stripChars = filter . flip notElem
      
parseA :: String -> [TestA]
parseA = parse . lines
  where
    intlist = map read . words
    clean = intlist . drop 7 . stripChars ",]["
    parse (a:b:c:rest) = (TestA (clean a) (intlist b) (clean c))
                       : parse (drop 1 rest)
    parse _ = []

solveA :: String -> Int
solveA = sum . map (test) . parseA 
  where
    test (TestA is (_:inst) os) = if ((length . filter (run) $ [0..15]) >= 3)
                              then 1 else 0
      where
        run opcode = os == (exec is (opcode : inst))

solveB :: String -> Int
solveB = head . run [0,0,0,0] . map (map (read) . words) . lines
  where
    run reg [] = reg
    run reg (x:xs) = run (exec reg x) xs

findOpcodes :: String -> IO ()
findOpcodes input = do
  let options = map (\x -> filter (\(y,x) -> x) . zip [0..15] $ x)
              . foldl (test) (replicate 16 (replicate 16 True))
              . parseA $ input
  print options
  where
    test hist (TestA is (realop:inst) os) = foldl (update) hist $ [0..15]
      where
        run opcode = os == (exec is (opcode : inst))
        update hist opcode
          | run opcode = hist
          | otherwise = replaceAt2 hist realop opcode False

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
  inputa <- readFile "inputa.txt"
  inputb <- readFile "inputb.txt"
  -- Would be nice to do this automatically. for now i just resolved it by hand
  -- putStrLn "Detecting opcodes:"
  -- findOpcodes $ inputa
  putStrLn "Solution for A:"
  print . solveA $ inputa
  putStrLn "Solution for B:"
  print . solveB $ inputb