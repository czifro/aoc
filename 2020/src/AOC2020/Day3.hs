module AOC2020.Day3 ( linearizeMap
                    , traverseMap
                    , processMap
                    , solve
                    , LinearMap (..)
                    , TraverseRules (..)
                    ) where

import Data.List

{-

--- Day 3: Toboggan Trajectory ---

With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.

Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#

These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:

..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:

From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.

The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:

..##.........##.........##.........##.........##.........##.......  --->
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

In this example, traversing the map using this slope would cause you to encounter 7 trees.

Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?

--- Part Two ---

Time to check the rest of the slopes - you need to minimize the probability of a sudden arboreal stop, after all.

Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:

    Right 1, down 1.
    Right 3, down 1. (This is the slope you already checked.)
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.

In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.

What do you get if you multiply together the number of trees encountered on each of the listed slopes?

-}

type MapSegmentWidth = Int
type MapHeight = Int
data LinearMap = LinearMap MapSegmentWidth MapHeight String
               deriving (Show, Eq)
type TraversedPoints = [Char]
type TraverseJump = Int
type TraverseShift = Int
data TraverseRules = TraverseRules TraverseJump TraverseShift

linearizeMap :: TraverseRules -> [String] -> LinearMap
linearizeMap (TraverseRules _ shiftBy) map' = LinearMap width height linearMap
 where
   height = length map'
   width = (*height) $ (*shiftBy) $ length $ head map'
   linearMap = concat $ map (\(row) -> concat $ replicate (shiftBy * height) row) map'

traverseMap :: TraverseRules -> LinearMap -> TraversedPoints
traverseMap (TraverseRules jumpBy shiftBy) (LinearMap width height map') = concat $ map (\(step) -> take 1 $ drop (traversalShift * step) map') traversedPoints
 where
   traversalShift = (width * jumpBy) + shiftBy
   traversedPoints = [1..height]

countTreesVisited :: TraversedPoints -> Int
countTreesVisited p = length $ filter (=='#') p

processMap :: TraverseRules -> [String] -> Int
processMap rules = countTreesVisited . (traverseMap rules) . (linearizeMap rules)

solvePart1 :: IO ()
solvePart1 = putStr . show . (processMap rules) . lines =<< readFile "inputs/day3.txt"
 where
   rules = TraverseRules 1 3

solvePart2 :: IO ()
solvePart2 = putStr . show . (foldl (*) 1) . (\(input) -> map (\(rules) -> processMap rules input) rulesSet) . lines =<< readFile "inputs/day3.txt"
 where
   rulesSet =
     [ TraverseRules 1 1
     , TraverseRules 1 3
     , TraverseRules 1 5
     , TraverseRules 1 7
     , TraverseRules 2 1
     ]

solve :: IO ()
solve = do
  putStr "Part 1: "
  solvePart1
  putStr "\n"
  putStr "Part 2: "
  solvePart2
