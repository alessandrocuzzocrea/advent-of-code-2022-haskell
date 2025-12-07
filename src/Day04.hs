module Day04 (part1, part2) where

import qualified Data.Set as Set
import Data.Set (Set)

type Coord = (Int, Int)

parse :: String -> Set Coord
parse input = Set.fromList
    [ (r, c)
    | (r, line) <- zip [0..] (lines input)
    , (c, char) <- zip [0..] line
    , char == '@'
    ]

neighbors :: Coord -> [Coord]
neighbors (r, c) =
    [ (r + dr, c + dc)
    | dr <- [-1, 0, 1]
    , dc <- [-1, 0, 1]
    , not (dr == 0 && dc == 0)
    ]

countNeighbors :: Set Coord -> Coord -> Int
countNeighbors grid coord =
    length . filter (`Set.member` grid) $ neighbors coord

part1 :: String -> Int
part1 input =
    let grid = parse input
    in Set.size $ Set.filter (\c -> countNeighbors grid c < 4) grid

part2 :: String -> Int
part2 input = solvePart2 (parse input)

solvePart2 :: Set Coord -> Int
solvePart2 grid =
    let removable = Set.filter (\c -> countNeighbors grid c < 4) grid
    in if Set.null removable
       then 0
       else Set.size removable + solvePart2 (Set.difference grid removable)
