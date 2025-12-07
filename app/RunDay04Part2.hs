import Day04 (part2)

main :: IO ()
main = do
    input <- readFile "inputs/day04.txt"
    print (part2 input)
