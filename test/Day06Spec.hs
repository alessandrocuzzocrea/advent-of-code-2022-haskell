module Day06Spec (spec) where

import Test.Hspec
import Day06

exampleInput :: String
exampleInput = unlines
    [ "123 328  51 64 "
    , " 45 64  387 23 "
    , "  6 98  215 314"
    , "*   +   *   +  "
    ]

spec :: Spec
spec = do
    describe "part1" $ do
        it "works with example" $
            part1 exampleInput `shouldBe` 4277556

    describe "part2" $ do
        it "works with example" $
            part2 exampleInput `shouldBe` 3263827
