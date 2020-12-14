module AOC2020.Day6Spec
  ( spec
  )
where

import           Test.Hspec
import           AOC2020.Day6                   ( countUniqueQuestionsAnswered
                                                , countSameQuestionsAnswered
                                                )

testInput :: String
testInput = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"

expected1 :: Int
expected1 = 11

expected2 :: Int
expected2 = 6

spec :: Spec
spec = do
  describe "<AOC2020.Day6>" $ do
    it "should correctly count unique questions answered" $ do
      countUniqueQuestionsAnswered testInput `shouldBe` expected1
    it "should correctly count same questions answered" $ do
      countSameQuestionsAnswered testInput `shouldBe` expected2
