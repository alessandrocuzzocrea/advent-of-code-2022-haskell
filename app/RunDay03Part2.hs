import Day03 (part2)

main :: IO ()
main = do
    input <- readFile "inputs/day03.txt"
    print (part2 input)
