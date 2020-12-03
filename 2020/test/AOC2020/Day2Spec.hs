module AOC2020.Day2Spec (spec) where

import Test.Hspec
import AOC2020.Day2 ( countValidPasswords
                    , parsePasswordPolicy
                    , parsePasswordEntry
                    , isValidPassword1
                    , isValidPassword2
                    , PasswordPolicy (..)
                    , Password (..)
                    )

testInput :: [String]
testInput =
  [ "1-3 a: abcde"
  , "1-3 b: cdefg"
  , "2-9 c: ccccccccc"
  ]

expected1 :: Int
expected1 = 2

expected2 :: Int
expected2 = 1

spec :: Spec
spec = do
  describe "<AOC2020.Day2>" $ do
    it "should parse password policy" $ do
      parsePasswordPolicy "1-3 a" `shouldBe` (PasswordPolicy 1 3 'a')
    it "should parse password entry" $ do
      parsePasswordEntry "1-3 a: abcde" `shouldBe` ((PasswordPolicy 1 3 'a'), (Password "abcde"))
    it "should return password as valid" $ do
      isValidPassword1 ((PasswordPolicy 1 3 'a'), (Password "abcde")) `shouldBe` True
    it "should return password as invalid" $ do
      isValidPassword1 ((PasswordPolicy 1 3 'b'), (Password "cdefg")) `shouldBe` False
    it "should correctly count valid passwords" $ do
      countValidPasswords isValidPassword1 testInput `shouldBe` expected1
    it "should return password as valid" $ do
      isValidPassword2 ((PasswordPolicy 1 3 'a'), (Password "abcde")) `shouldBe` True
    it "should return password as invalid" $ do
      isValidPassword2 ((PasswordPolicy 1 3 'b'), (Password "abade")) `shouldBe` False
    it "should correctly count valid passwords" $ do
      countValidPasswords isValidPassword2 testInput `shouldBe` expected2
