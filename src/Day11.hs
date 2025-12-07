module Day11 (part1, part2) where

import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.List (sort, stripPrefix)
import Data.Maybe (fromJust)

data Monkey = Monkey
  { mId :: Int,
    mItems :: [Int],
    mOperation :: Int -> Int,
    mDivisor :: Int,
    mTrueDest :: Int,
    mFalseDest :: Int,
    mInspections :: Int
  }

instance Show Monkey where
  show m = "Monkey " ++ show (mId m) ++ ": " ++ show (mItems m) ++ ", inspected " ++ show (mInspections m)

parseInput :: String -> IntMap Monkey
parseInput input = M.fromList $ map parseMonkey (splitBlocks (lines input))

splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks lines' =
  let (block, rest) = break (== "") lines'
   in block : splitBlocks (drop 1 rest)

parseMonkey :: [String] -> (Int, Monkey)
parseMonkey lines' =
  let idLine = lines' !! 0
      itemsLine = lines' !! 1
      opLine = lines' !! 2
      testLine = lines' !! 3
      trueLine = lines' !! 4
      falseLine = lines' !! 5

      mId = read (init (last (words idLine))) :: Int
      items = map read $ words $ filter (\c -> isDigit c || c == ' ') (dropWhile (/= ':') itemsLine)

      opWords = words (dropWhile (/= '=') opLine)
      op = parseOp (opWords !! 2) (opWords !! 3)

      divisor = read (last (words testLine)) :: Int
      trueDest = read (last (words trueLine)) :: Int
      falseDest = read (last (words falseLine)) :: Int
   in (mId, Monkey mId items op divisor trueDest falseDest 0)

parseOp :: String -> String -> (Int -> Int)
parseOp "*" "old" = \x -> x * x
parseOp "+" "old" = \x -> x + x
parseOp "*" val = \x -> x * (read val)
parseOp "+" val = \x -> x + (read val)
parseOp _ _ = error "Unknown operation"

part1 :: String -> Int
part1 input =
  let monkeys = parseInput input
      finalMonkeys = iterate (playRound (`div` 3)) monkeys !! 20
      inspections = reverse $ sort $ map mInspections (M.elems finalMonkeys)
   in product (take 2 inspections)

playRound :: (Int -> Int) -> IntMap Monkey -> IntMap Monkey
playRound reliefFn monkeys = foldl (turn reliefFn) monkeys (M.keys monkeys)

turn :: (Int -> Int) -> IntMap Monkey -> Int -> IntMap Monkey
turn reliefFn monkeys currentMonkeyId =
  let monkey = monkeys M.! currentMonkeyId
      items = mItems monkey
      monkeysAfterInspect = M.adjust (\m -> m {mItems = [], mInspections = mInspections m + length items}) currentMonkeyId monkeys
   in foldl (processItem reliefFn monkey) monkeysAfterInspect items

processItem :: (Int -> Int) -> Monkey -> IntMap Monkey -> Int -> IntMap Monkey
processItem reliefFn sourceMonkey monkeys item =
  let worryLevel = (mOperation sourceMonkey item)
      boredWorryLevel = reliefFn worryLevel
      targetId =
        if boredWorryLevel `mod` mDivisor sourceMonkey == 0
          then mTrueDest sourceMonkey
          else mFalseDest sourceMonkey
   in M.adjust (\m -> m {mItems = mItems m ++ [boredWorryLevel]}) targetId monkeys

part2 :: String -> Int
part2 _ = 0
