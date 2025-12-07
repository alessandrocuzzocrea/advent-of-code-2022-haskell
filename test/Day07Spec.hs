module Day07Spec (spec) where

import Test.Hspec
import Day07 (part1, part2)

exampleInput :: String
exampleInput = unlines
    [ ".......S......."
    , "..............."
    , ".......^......."
    , "..............."
    , "......^.^......"
    , "..............."
    , ".....^.^.^....."
    , "..............."
    , "....^.^...^...."
    , "..............."
    , "...^.^...^.^..."
    , "..............."
    , "..^...^.....^.."
    , "..............."
    , ".^.^.^.^.^...^."
    , "..............."
    ]

spec :: Spec
spec = do
    describe "part1" $ do
        it "works with example" $
            part1 exampleInput `shouldBe` 21

    describe "part2" $ do
        it "works with example" $
            part2 exampleInput `shouldBe` 40
