module Main where

import Test.HUnit
import Debug.Trace
import Data.List

--------------------------------------------------------------------------------
-- Day specific part. This is where the implementation happens
--------------------------------------------------------------------------------

testInput = "Immune System:\n\
            \17 units each with 5390 hit points (weak to radiation, bludgeoning\
            \) with an attack that does 4507 fire damage at initiative 2\n\
            \989 units each with 1274 hit points (immune to fire; weak to \
            \bludgeoning, slashing) with an attack that does 25 slashing damage\
            \ at initiative 3\n\
            \\n\
            \Infection:\n\
            \801 units each with 4706 hit points (weak to radiation) with an \
            \ attack that does 116 bludgeoning damage at initiative 1\n\
            \4485 units each with 2961 hit points (immune to radiation; weak to\
            \ fire, cold) with an attack that does 12 slashing damage at \
            \initiative 4"

tests = TestList [
    addTest solveA testInput 5216
  ]

data Group = Group {
    groupInitiative :: Int,
    groupSide :: Int,
    groupCount :: Int,
    groupHp :: Int,
    groupWeaknesses :: [String],
    groupImmunities :: [String],
    groupDamage :: Int,
    groupAttackType :: String,
    groupTarget :: Int
  } deriving (Show, Eq, Ord)

data Battle = Battle {
    battleGroups :: [Group]
  } deriving (Show)


readWI :: String -> [String] -> [String]
readWI what ws = scan' ws
  where
    scan' [] = []
    scan' (w:ws)
      | w == what = read' [] ws
      | otherwise = scan' ws
    read' xs [] = xs
    read' xs (w:ws)
      -- skip "to"
      | w == "to" = read' xs ws
      -- terminate if either "immune" or "weak" is detected
      | w == "immune" || w == "weak" = xs
      -- otherwise this is a legit buff or debuff
      | otherwise = read' (w:xs) ws
  
parse :: String -> Battle
parse input = battle
  where
    readGroup side l = Group {
        groupSide = side,
        groupCount = read (ws!!0),
        groupHp = read (ws!!4),
        groupWeaknesses = readWI "weak" wilist,
        groupImmunities = readWI "immune" wilist,
        groupDamage = read (rws!!5),
        groupAttackType = rws!!4,
        groupInitiative = read (rws!!0),
        groupTarget = -1
      }
      where
        ws = words . filter (not . (`elem` "(),;")) $ l
        rws = reverse $ ws
        wilist = reverse . drop 11 . reverse . drop 7 $ ws
    readGroups b _ [] = b
    readGroups b side (l:ls)
      | length l == 0 = readGroups b side ls
      | l == "Immune System:" = readGroups b 0 ls
      | l == "Infection:" = readGroups b 1 ls
      | otherwise = readGroups (b {battleGroups=groups}) side ls 
      where groups = battleGroups b ++ [readGroup side l]
    battle = readGroups (Battle []) 0 . lines $ input

step :: Battle -> Battle
step b = b { battleGroups=g'}
  where
    g = battleGroups b
    g' = g

solveA :: String -> Int
solveA = countSurvivors . parse
  where
    countSurvivors b = sum . map (\g -> (groupCount g)) $ battleGroups b

effectivePower :: Group -> Int
effectivePower g = groupCount g * groupDamage g

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
  -- print . solveA $ input
  putStrLn "Solution for B:"
  -- print . solveB $ input
  -- print . parse $ testInput
  print . reverse . sort . map (\g -> (effectivePower g, g)). battleGroups . parse $ testInput
  -- print . step . parse $ testInput