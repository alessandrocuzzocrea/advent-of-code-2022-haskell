import Day03 (part1)

main :: IO ()
main = do
    input <- readFile "inputs/day03.txt"
    print (part1 input)
