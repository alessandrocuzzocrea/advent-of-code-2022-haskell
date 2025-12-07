module Day01 (part1, part2) where

import Data.Char (isDigit)

-- Types
data Direction = L | R
  deriving (Show, Eq)

data Instruction = Instr Direction Int
  deriving (Show, Eq)

type Pos = Int

-- Parsing
parse :: String -> Instruction
parse (c:rest) = 
    let dir = case c of
                'L' -> L
                'R' -> R
                _   -> error "Invalid direction"
        n = read (filter isDigit rest)
    in Instr dir n
parse _ = error "Invalid input"

-- Part 1 Logic
step1 :: (Pos, Int) -> Instruction -> (Pos, Int)
step1 (pos, count) (Instr dir dist) =
    let newPos = case dir of
                    L -> (pos - dist) `mod` 100
                    R -> (pos + dist) `mod` 100
        count' = if newPos == 0 then count + 1 else count
    in (newPos, count')

part1 :: String -> Int
part1 input =
    let instructions = map parse (lines input)
        (_, count)   = foldl step1 (50, 0) instructions
    in count

-- Part 2 Logic
dialSize :: Int
dialSize = 100

normalize :: Int -> Int
normalize x = (x `mod` dialSize + dialSize) `mod` dialSize

step2 :: (Int, Int) -> Instruction -> (Int, Int)
step2 (pos, count) (Instr dir n) =
    let signed = case dir of
                    L -> -n
                    R ->  n
        end = pos + signed
        
        crossings = if signed > 0
                    then (end `div` 100) - (pos `div` 100)
                    else ((pos - 1) `div` 100) - ((end - 1) `div` 100)
        
        pos' = normalize end
    in (pos', count + crossings)

part2 :: String -> Int
part2 input =
    let instructions = map parse (lines input)
        (_, total)   = foldl step2 (50, 0) instructions
    in total
