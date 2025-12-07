import Day02 (part2)

main :: IO ()
main = do
    input <- readFile "inputs/day02.txt"
    print (part2 input)
