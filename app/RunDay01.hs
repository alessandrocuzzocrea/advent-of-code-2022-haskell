import Day01 (part1)

main :: IO ()
main = do
    input <- readFile "inputs/day01.txt"
    print (part1 input)
