import Day04 (part1)

main :: IO ()
main = do
    input <- readFile "inputs/day04.txt"
    print (part1 input)
