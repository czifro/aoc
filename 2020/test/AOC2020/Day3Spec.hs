module AOC2020.Day3Spec
  ( spec
  )
where

import           Test.Hspec
import           AOC2020.Day3                   ( linearizeMap
                                                , traverseMap
                                                , processMap
                                                , LinearMap(..)
                                                , TraverseRules(..)
                                                )

testInput :: [String]
testInput =
  [ "..##......."
  , "#...#...#.."
  , ".#....#..#."
  , "..#.#...#.#"
  , ".#...##..#."
  , "..#.##....."
  , ".#.#.#....#"
  , ".#........#"
  , "#.##...#..."
  , "#...##....#"
  , ".#..#...#.#"
  ]

rules :: TraverseRules
rules = TraverseRules 1 3

rulesSet :: [TraverseRules]
rulesSet =
  [ TraverseRules 1 1
  , TraverseRules 1 3
  , TraverseRules 1 5
  , TraverseRules 1 7
  , TraverseRules 2 1
  ]

expected1 :: Int
expected1 = 7

expected2 :: Int
expected2 = 336

spec :: Spec
spec = do
  describe "<AOC2020.Day3>" $ do
    it "should linearize map" $ do
      linearizeMap rules testInput
        `shouldBe` (LinearMap
                     (3 * 11 * 11)
                     11
                     (concatMap (\r -> concat $ replicate (3 * 11) r) testInput)
                   )
    it "should traverse map correctly" $ do
      traverseMap
          rules
          (LinearMap
            (3 * 11 * 11)
            11
            (concatMap (\r -> concat $ replicate (3 * 11) r) testInput)
          )
        `shouldBe` ".#.##.####"
    it "should visit correct number of trees" $ do
      processMap rules testInput `shouldBe` expected1
    it "should calculate correct number" $ do
      (product . (\input -> map (\rules' -> processMap rules' input) rulesSet))
          testInput
        `shouldBe` expected2
