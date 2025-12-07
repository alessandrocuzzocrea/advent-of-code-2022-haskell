module Main where

import Day07 (part1, part2)

main :: IO ()
main = do
    input <- readFile "inputs/day07.txt"
    putStrLn "Part 1:"
    print (part1 input)
    putStrLn "Part 2:"
    print (part2 input)
