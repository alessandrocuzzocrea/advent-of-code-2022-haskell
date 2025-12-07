module Day07 (part1, part2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

type Coord = (Int, Int)
type Grid = [String]

part1 :: String -> Int
part1 input = solvePart1 grid start
  where
    grid = lines input
    start = findStart grid

findStart :: Grid -> Coord
findStart grid = head [(r, c) | (r, row) <- zip [0..] grid, (c, cell) <- zip [0..] row, cell == 'S']

solvePart1 :: Grid -> Coord -> Int
solvePart1 grid (startRow, startCol) = simulate grid (startRow) (Set.singleton startCol) 0

simulate :: Grid -> Int -> Set Int -> Int -> Int
simulate grid currentRow activeCols splitCount
    | currentRow >= length grid = splitCount
    | Set.null activeCols = splitCount
    | otherwise = simulate grid (currentRow + 1) nextCols newSplitCount
  where
    currentNbCols = length (grid !! currentRow)
    
    -- process current row
    -- filter out columns that are out of bounds    
    validCols = Set.filter (\c -> c >= 0 && c < currentNbCols) activeCols
    
    -- determine what happens for each active column
    (nextCols, addedSplits) = foldl processCol (Set.empty, 0) validCols
    
    newSplitCount = splitCount + addedSplits
    
    rowStr = grid !! currentRow
    
    processCol :: (Set Int, Int) -> Int -> (Set Int, Int)
    processCol (accSet, accSplits) c =
        let cell = rowStr !! c
        in case cell of
            '^' -> (Set.insert (c - 1) $ Set.insert (c + 1) accSet, accSplits + 1)
            '.' -> (Set.insert c accSet, accSplits)
            'S' -> (Set.insert c accSet, accSplits) -- s is empty space, passes through
            _   -> (Set.insert c accSet, accSplits) -- treat other chars as empty space

part2 :: String -> Int
part2 input = fromIntegral $ solvePart2 grid start
  where
    grid = lines input
    start = findStart grid

solvePart2 :: Grid -> Coord -> Integer
solvePart2 grid (startRow, startCol) = simulateQuantum grid (startRow) (Map.singleton startCol 1) 0

simulateQuantum :: Grid -> Int -> Map.Map Int Integer -> Integer -> Integer
simulateQuantum grid currentRow activeCols finishedCount
    | currentRow >= length grid = finishedCount + sum (Map.elems activeCols)
    | Map.null activeCols = finishedCount
    | otherwise = simulateQuantum grid (currentRow + 1) nextCols newFinishedCount
  where
    currentNbCols = length (grid !! currentRow)
    rowStr = grid !! currentRow
    
    -- current row
    (nextCols, addedFinished) = Map.foldrWithKey processCol (Map.empty, 0) activeCols
    
    newFinishedCount = finishedCount + addedFinished
    
    processCol :: Int -> Integer -> (Map.Map Int Integer, Integer) -> (Map.Map Int Integer, Integer)
    processCol c count (accNext, accFin) =
        if c < 0 || c >= currentNbCols
        then (accNext, accFin + count) -- out of bounds
        else
            let cell = rowStr !! c
            in case cell of
                '^' -> 
                    let leftC = c - 1
                        rightC = c + 1
                        -- check bounds for children
                        (accNext1, accFin1) = addParticle leftC count currentNbCols (accNext, accFin)
                        (accNext2, accFin2) = addParticle rightC count currentNbCols (accNext1, accFin1)
                    in (accNext2, accFin2)
                _ -> -- passes through (., S, etc)
                    -- moves to same column in next row
                    addParticle c count currentNbCols (accNext, accFin)

    addParticle :: Int -> Integer -> Int -> (Map.Map Int Integer, Integer) -> (Map.Map Int Integer, Integer)
    addParticle c count width (accMap, accFin)
        | c < 0 || c >= width = (accMap, accFin + count)
        | otherwise = (Map.insertWith (+) c count accMap, accFin)
