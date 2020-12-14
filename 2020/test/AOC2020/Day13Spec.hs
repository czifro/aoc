module AOC2020.Day13Spec
  ( spec
  )
where

import           Test.Hspec
import           AOC2020.Day13                  ( magicNumberPart1
                                                , magicNumberPart2
                                                )

testInput1 :: String
testInput1 = "939\n7,13,x,x,59,x,31,19"

expected1 :: Int
expected1 = 295

testInput2 :: [String]
testInput2 =
  [ "17,x,13,19"
  , "67,7,59,61"
  , "67,x,7,59,61"
  , "67,7,x,59,61"
  , "1789,37,47,1889"
  ]

expected2 :: [Integer]
expected2 = [3417, 754018, 779210, 1261476, 1202161486]

spec :: Spec
spec = do
  describe "<AOC2020.Day13>" $ do
    it "should calculate magic number part 1" $ do
      magicNumberPart1 testInput1 `shouldBe` expected1
    it "should calculate magic number part 2" $ do
      map magicNumberPart2 testInput2 `shouldBe` expected2
