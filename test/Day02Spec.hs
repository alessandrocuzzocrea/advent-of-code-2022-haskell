module Day02Spec (spec) where

import Test.Hspec
import Day02

exampleInput :: String
exampleInput = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

spec :: Spec
spec = do
    describe "part1" $ do
        it "works with example" $
            part1 exampleInput `shouldBe` 1227775554

    describe "part2" $ do
        it "works with example" $
            part2 exampleInput `shouldBe` 4174379265
