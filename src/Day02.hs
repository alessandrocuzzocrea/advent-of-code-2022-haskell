module Day02 (part1, part2) where

import Data.Char (isDigit)

-- parse "11-22,95-115" into [(11,22), (95,115)]
parse :: String -> [(Int, Int)]
parse input = 
    let clean c = if c == ',' || c == '-' || c == '\n' then ' ' else c
        nums = map read $ words $ map clean input
    in toPairs nums
  where
    toPairs (x:y:xs) = (x,y) : toPairs xs
    toPairs _ = []

-- generate invalid IDs (N = X concatenated with X) within a range by iterating possible lengths of X
invalidsInRange :: (Int, Int) -> [Int]
invalidsInRange (lo, hi) =
    let 
        -- determine the maximum length of X (half the digits of hi) to generate candidates
        maxDigits = length (show hi)
        maxHalfLen = maxDigits `div` 2
        
        candidates = do
            len <- [1 .. maxHalfLen]
            let startX = 10 ^ (len - 1)
                endX   = (10 ^ len) - 1
            x <- [startX .. endX]
            let n = x * (10 ^ len) + x
            return n
            
    in filter (\n -> n >= lo && n <= hi) candidates
    
-- part 2: Invalid IDs are formed by repeating a sequence X at least twice
invalidsInRangePart2 :: (Int, Int) -> [Int]
invalidsInRangePart2 (lo, hi) =
    let 
        maxDigits = length (show hi)
        
        candidates = do
            -- length of the base sequence X
            len <- [1 .. maxDigits `div` 2]
            
            -- number of repetitions k
            k <- [2 .. maxDigits `div` len]
            
            let startX = 10 ^ (len - 1)
                endX   = (10 ^ len) - 1
            
            x <- [startX .. endX]
            
            -- construct N by repeating X, k times
            let n = constructRepetition x len k
            return n
            
    in filter (\n -> n >= lo && n <= hi) candidates

constructRepetition :: Int -> Int -> Int -> Int
constructRepetition x len k =
    let shift = 10 ^ len
        go 0 acc = acc
        go i acc = go (i - 1) (acc * shift + x)
    in go (k - 1) x

part1 :: String -> Int
part1 input = 
    let ranges = parse input
        invalids = concatMap invalidsInRange ranges
    in sum invalids

part2 :: String -> Int
part2 input = 
    let ranges = parse input
        -- deduplicate per range because a number like 111111 can be formed by repeating 1 (6x), 11 (3x), or 111 (2x)
        invalids = concatMap (unique . invalidsInRangePart2) ranges
    in sum invalids

unique :: [Int] -> [Int]
unique xs = 
    let sorted = mergeSort xs
    in nubSorted sorted

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
    in merge (mergeSort left) (mergeSort right)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y    = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

nubSorted :: Eq a => [a] -> [a]
nubSorted (x:y:xs)
    | x == y    = nubSorted (y:xs)
    | otherwise = x : nubSorted (y:xs)
nubSorted xs = xs
