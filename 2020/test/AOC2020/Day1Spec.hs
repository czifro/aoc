module AOC2020.Day1Spec (spec) where

import Test.Hspec
import AOC2020.Day1 ( computeMagicNumber1
                    , computeMagicNumber2
                    )

testInput :: [Int]
testInput = [1721, 979, 366, 299, 675, 1456]

expected1 :: Int
expected1 = 514579

expected2 :: Int
expected2 = 241861950

spec :: Spec
spec = do
  describe "<AOC2020.Day1>" $ do
    it "part 1 should compute correct magic number" $ do
      computeMagicNumber1 testInput `shouldBe` expected1
    it "part 2 should compute correct magic number" $ do
      computeMagicNumber2 testInput `shouldBe` expected2
