module Day03Spec (spec) where

import Test.Hspec
import Day03

exampleInput :: String
exampleInput = unlines
    [ "987654321111111"
    , "811111111111119"
    , "234234234234278"
    , "818181911112111"
    ]

spec :: Spec
spec = do
    describe "part1" $ do
        it "works with example" $
            part1 exampleInput `shouldBe` 357

    describe "part2" $ do
        it "works with example" $
            part2 exampleInput `shouldBe` 3121910778619
