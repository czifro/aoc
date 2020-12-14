module AOC2020.Day8Spec
  ( spec
  )
where

import           Test.Hspec
import           AOC2020.Day8                   ( runBootCode
                                                , fixAndRunBootCode
                                                )

testInput :: [String]
testInput =
  [ "nop +0"
  , "acc +1"
  , "jmp +4"
  , "acc +3"
  , "jmp -3"
  , "acc -99"
  , "acc +1"
  , "jmp -4"
  , "acc +6"
  ]

expected1 :: Int
expected1 = 5

expected2 :: Int
expected2 = 8

spec :: Spec
spec = do
  describe "<AOC2020.Day8>" $ do
    it "should have correct accumulated value" $ do
      runBootCode testInput `shouldBe` expected1
    it "should terminate with correct accumulated value" $ do
      fixAndRunBootCode testInput `shouldBe` expected2
