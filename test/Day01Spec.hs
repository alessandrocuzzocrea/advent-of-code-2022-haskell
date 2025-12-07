module Day01Spec (spec) where

import Test.Hspec
import Day01 (part1, part2)

exampleInput :: String
exampleInput = unlines
    [ "L68", "L30", "R48", "L5", "R60"
    , "L55", "L1", "L99", "R14", "L82"
    ]

spec :: Spec
spec = do
    describe "part1" $ do
        it "works with example" $
            part1 exampleInput `shouldBe` 3

    describe "part2" $ do
        it "works with example" $
            part2 exampleInput `shouldBe` 6
