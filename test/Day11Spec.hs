module Day11Spec (spec) where

import Test.Hspec
import Day11 (part1, part2)

spec :: Spec
spec = do
  describe "Part 1" $ do
    it "works for example" $ do
      part1 "" `shouldBe` 0
  
  describe "Part 2" $ do
    it "works for example" $ do
      part2 "" `shouldBe` 0
