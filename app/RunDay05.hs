import Day05 (part1)

main :: IO ()
main = do
    input <- readFile "inputs/day05.txt"
    print (part1 input)
