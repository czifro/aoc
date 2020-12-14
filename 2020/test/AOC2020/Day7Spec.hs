module AOC2020.Day7Spec
  ( spec
  , testInput1
  , testInput2
  )
where

import           Test.Hspec
import           AOC2020.Day7                   ( countVerticesLeadingTo
                                                , getMaxFlow
                                                )

testInput1 :: String
testInput1 = unlines
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]

testInput2 :: String
testInput2 = unlines
  [ "shiny gold bags contain 2 dark red bags."
  , "dark red bags contain 2 dark orange bags."
  , "dark orange bags contain 2 dark yellow bags."
  , "dark yellow bags contain 2 dark green bags."
  , "dark green bags contain 2 dark blue bags."
  , "dark blue bags contain 2 dark violet bags."
  , "dark violet bags contain no other bags."
  ]

expected1 :: Int
expected1 = 4

expected2a :: Int
expected2a = 32

expected2b :: Int
expected2b = 126

spec :: Spec
spec = do
  describe "<AOC2020.Day7>" $ do
    it "should count correct number of parent nodes" $ do
      countVerticesLeadingTo "shiny gold" testInput1 `shouldBe` expected1
    it "should compute correct max flow - a" $ do
      getMaxFlow "shiny gold" testInput1 `shouldBe` expected2a
    it "should compute correct max flow - b" $ do
      getMaxFlow "shiny gold" testInput2 `shouldBe` expected2b
