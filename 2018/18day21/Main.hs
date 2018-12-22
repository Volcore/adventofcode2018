module Main where

import Test.HUnit
import Debug.Trace
import Data.Bits
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

tests = TestList [
  ]

data IS = IS String Int Int Int deriving (Show)

data VM = VM {
    vmIpIdx :: Int,
    vmIp :: Int,
    vmRegisters :: [Int],
    vmInstructions :: [IS]
  } deriving (Show)

parse :: String -> VM
parse input = (VM ipidx 0 reg inst)
  where
    ls = lines $ input
    ipidx = read ((words . head $ ls)!!1)
    reg = [0,0,0,0,0,0]
    inst = foldr (readInst) [] . drop 1 $ ls
    readInst line is = (IS (ws!!0) (r 1) (r 2) (r 3)) : is
      where
        ws = words $ line
        r x = read (ws!!x)

step :: VM -> VM
step vm
  -- default execution path
  | otherwise = vm { vmIp = ip', vmRegisters = reg' }
  where
    -- Normal execution
    ip = vmIp vm
    ip' = reg'!!(vmIpIdx vm) + 1
    reg = replaceAt (vmRegisters vm) (vmIpIdx vm) ip
    reg' = exec reg ((vmInstructions vm)!!ip)

runA :: VM -> VM
runA vm
  -- | trace (show (vmIp vm) ++ " " ++ show (vmRegisters vm)) False = undefined
  | (vmIp vm) == 29 = vm
  | (vmIp vm) >= length (vmInstructions vm) = vm
  | otherwise = runA . step $ vm

runB :: Set Int -> VM -> Int
runB set vm
  -- | trace (show (vmIp vm) ++ " " ++ show (vmRegisters vm)) False = undefined
  | (vmIp vm) == 29 && Set.member (reg!!2) set = reg!!2
  | (vmIp vm) == 29 = trace (show (reg!!2) ++ " " ++ show set)
                      doStep
                    . Set.insert (reg!!2) $ set
  | (vmIp vm) >= length (vmInstructions vm) = 0
  | otherwise = doStep $ set
    where
      reg = vmRegisters vm
      doStep set' = runB set' . step $ vm

exec :: [Int] -> IS -> [Int]
exec reg (IS op a b c)
  | op == "addr" = replaceAt reg c (reg!!a + reg!!b)
  | op == "seti" = replaceAt reg c (a)
  | op == "addi" = replaceAt reg c (reg!!a + b)
  | op == "mulr" = replaceAt reg c (reg!!a * reg!!b)
  | op == "muli" = replaceAt reg c (reg!!a * b)
  | op == "banr" = replaceAt reg c (reg!!a .&. reg!!b)
  | op == "bani" = replaceAt reg c (reg!!a .&. b)
  | op == "borr" = replaceAt reg c (reg!!a .|. reg!!b)
  | op == "bori" = replaceAt reg c (reg!!a .|. b)
  | op == "setr" = replaceAt reg c (reg!!a)
  | op == "gtir" = replaceAt reg c (if (a > reg!!b) then 1 else 0)
  | op == "gtri" = replaceAt reg c (if (reg!!a > b) then 1 else 0)
  | op == "gtrr" = replaceAt reg c (if (reg!!a > reg!!b) then 1 else 0)
  | op == "eqir" = replaceAt reg c (if (a == reg!!b) then 1 else 0)
  | op == "eqri" = replaceAt reg c (if (reg!!a == b) then 1 else 0)
  | op == "eqrr" = replaceAt reg c (if (reg!!a == reg!!b) then 1 else 0)
  | otherwise = trace ("Unknown opcode " ++ show op) reg

solveA :: String -> Int
solveA = head . drop 2 . vmRegisters . runA . parse

setRegister :: Int -> Int -> VM -> VM
setRegister idx val vm = vm { vmRegisters = replaceAt (vmRegisters vm) idx val }

solveB :: String -> Int
solveB = runB Set.empty . parse

replaceAt :: [a] -> Int -> a -> [a]
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
  print . solveA $ input
  putStrLn "Solution for B:"
  print . solveB $ input

-- Solution for 1: 13970209
-- Solution for 2: 

-- EQRR r[2] 13970209 == r[0] 0
-- Part 1: 13970209
-- 6823672 already present, stop. set size 10396
-- Part 2: 6267260
