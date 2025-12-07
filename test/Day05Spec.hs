module Day05Spec (spec) where

import Test.Hspec
import Day05

exampleInput :: String
exampleInput = unlines
    [ "3-5"
    , "10-14"
    , "16-20"
    , "12-18"
    , ""
    , "1"
    , "5"
    , "8"
    , "11"
    , "17"
    , "32"
    ]

spec :: Spec
spec = do
    describe "part1" $ do
        it "works with example" $
            part1 exampleInput `shouldBe` 3

    describe "part2" $ do
        it "works with example" $
            part2 exampleInput `shouldBe` 14
