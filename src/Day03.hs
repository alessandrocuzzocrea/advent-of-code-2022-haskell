module Day03 (part1, part2) where

part1 :: String -> Int
part1 input = sum $ map (solveBank 2) (lines input)

solveBank :: Int -> String -> Int
solveBank k s = read (solve k s)

-- recursive solver pick k digits from s to maximize the resulting number
solve :: Int -> String -> String
solve 0 _ = ""
solve k s =
    let -- last possible index to pick from while leaving enough chars for remaining k-1 digits
        lastIdx = length s - k
        candidates = take (lastIdx + 1) s
        
        -- pick largest digit if ties pick first occurrence to maximize remaining suffix
        maxDigit = maximum candidates
        
        -- find first occurrence of maxDigit
        (_, matchAndAfter) = break (== maxDigit) s
        
        -- recurse on the suffix following the chosen digit
        remainingS = tail matchAndAfter
        
    in maxDigit : solve (k - 1) remainingS

part2 :: String -> Int
part2 input = sum $ map (solveBank 12) (lines input)
