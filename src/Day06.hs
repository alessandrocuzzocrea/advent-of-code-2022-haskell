module Day06 (part1, part2) where

import Data.List (transpose, dropWhileEnd)
import Data.Char (isSpace, isDigit)

part1 :: String -> Int
part1 input = sum $ map solveProblem (splitProblems input)

splitProblems :: String -> [String]
splitProblems input =
    let lines' = lines input
        maxLength = maximum (map length lines')
        paddedLines = map (padRight maxLength) lines'
        cols = transpose paddedLines
        -- split cols by empty columns
        problemCols = splitByEmpty cols
    in map (unlines . transpose) problemCols

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

splitByEmpty :: [String] -> [[String]]
splitByEmpty [] = []
splitByEmpty cols =
    let (chunk, rest) = break (all isSpace) cols
        rest' = dropWhile (all isSpace) rest
    in if null chunk
       then splitByEmpty rest'
       else chunk : splitByEmpty rest'

solveProblem :: String -> Int
solveProblem s =
    let allWords = words s
        operator = head $ filter (\w -> w == "+" || w == "*") allWords
        numbers = map read $ filter (all isDigit) allWords
    in case operator of
        "+" -> sum numbers
        "*" -> product numbers
        _ -> 0

part2 :: String -> Int
part2 input = sum $ map solveProblemPart2 (splitProblemsPart2 input)

splitProblemsPart2 :: String -> [String]
splitProblemsPart2 input =
    let lines' = lines input
        maxLength = maximum (map length lines')
        paddedLines = map (padRight maxLength) lines'
        cols = transpose paddedLines
        -- split cols by empty columns
        problemCols = splitByEmpty cols
    in map (unlines . transpose) problemCols

solveProblemPart2 :: String -> Int
solveProblemPart2 s =
    let lines' = lines s
        -- the last line contains the operator
        operatorLine = last lines'
        operator = head $ filter (\c -> c == '+' || c == '*') operatorLine
        
        -- the other lines contain the digits
        digitLines = init lines'
        -- transpose to get columns numbers
        -- filter out columns that are just spaces
        cols = filter (not . all isSpace) (transpose digitLines)
        -- each column is a number read from top to bottom
        numbers = map readColumn cols
    in case operator of
        '+' -> sum numbers
        '*' -> product numbers
        _ -> 0

readColumn :: String -> Int
readColumn col = read (filter isDigit col)
