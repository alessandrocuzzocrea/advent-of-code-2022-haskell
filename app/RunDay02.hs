import Day02 (part1)

main :: IO ()
main = do
    input <- readFile "inputs/day02.txt"
    print (part1 input)
