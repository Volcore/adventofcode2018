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
    vmInstructions :: [IS],
    vmOptim :: Bool
  } deriving (Show)

parse :: String -> VM
parse input = (VM ipidx 0 reg inst False)
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
  -- Decompiled the program and figured out a faster solution
  -- See "notes" file for the derivation
  | vmOptim vm = setIp 9999
               . setRegister 0 (sum . filter (\x -> 10551331 `mod` x == 0) $ [1..10551331]) $ vm
  -- initial optimization that allowed me to see what values it's looking for
  | innerLoopCheck = vm { vmIp = 12, vmRegisters = regil }
  -- default execution path
  | otherwise = vm { vmIp = ip', vmRegisters = reg' }
  where
    -- Inner loop optimization
    innerLoopCheck = ip == 3 && (vmOptim vm)
    regil0
      | (reg!!5) `mod` (reg!!1) == 0 = reg!!0 + reg!!1
      | otherwise = reg!!0
    regil = [regil0, reg!!1, reg!!2, 1, reg!!5 + 1, reg!!5]
    -- Normal execution
    ip = vmIp vm
    ip' = reg'!!(vmIpIdx vm) + 1
    reg = replaceAt (vmRegisters vm) (vmIpIdx vm) ip
    reg' = exec reg ((vmInstructions vm)!!ip)

run :: VM -> VM
run vm
  -- | trace (show (vmIp vm) ++ " " ++ show (vmRegisters vm)) False = undefined
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

setIp :: Int -> VM -> VM
setIp ip vm = vm {vmIp = ip}

setRegister :: Int -> Int -> VM -> VM
setRegister idx val vm = vm { vmRegisters = replaceAt (vmRegisters vm) idx val }

setOptim :: VM -> VM
setOptim vm = vm { vmOptim = True }

solveB :: String -> Int
solveB = head . vmRegisters . run . setOptim . setRegister 0 1 . parse

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
