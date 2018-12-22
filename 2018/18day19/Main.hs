module Main where

import Test.HUnit
import Debug.Trace
import Data.Bits
import Data.List (intercalate)

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "#ip 0\n\
            \seti 5 0 1\n\
            \seti 6 0 2\n\
            \addi 0 1 0\n\
            \addr 1 2 3\n\
            \setr 1 0 0\n\
            \seti 8 0 4\n\
            \seti 9 0 5"

tests = TestList [
  addTest solveA testInput 6
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
step vm = vm { vmIp = ip', vmRegisters = reg' }
  where
    ip' = reg'!!(vmIpIdx vm) + 1
    reg = replaceAt (vmRegisters vm) (vmIpIdx vm) (vmIp vm)
    reg' = exec reg ((vmInstructions vm)!!(vmIp vm))

run :: VM -> VM
run vm
  | (vmIp vm) >= length (vmInstructions vm) = vm
  | otherwise = run . step $ vm

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
solveA = head . vmRegisters . run . parse

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
  -- putStrLn "Solution for B:"
  -- print . solveB $ inputb
  -- print . parse $ testInput
  -- print . run . parse $ testInput
  -- print . step . parse $ testInput
  -- print . step . step . parse $ testInput
  -- print . step . step . step . parse $ testInput