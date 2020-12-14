module AOC2020.Day5Spec
  ( spec
  )
where

import           Test.Hspec
import           AOC2020.Day5                   ( calculateSeatId )

testInput :: [String]
testInput = ["FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]

expected1 :: [Int]
expected1 = [357, 567, 119, 820]

spec :: Spec
spec = do
  describe "<AOC2020.Day5>" $ do
    it "should calculate correct seat id" $ do
      map calculateSeatId testInput `shouldBe` expected1
