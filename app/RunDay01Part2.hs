import Day01 (part2)

main :: IO ()
main = do
    input <- readFile "inputs/day01.txt"
    print (part2 input)
