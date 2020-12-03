module AOC2020.Day1 ( computeMagicNumber1
                    , computeMagicNumber2
                    , solve
                    ) where

import System.IO
import Control.Monad

{-
--- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation at a nice resort on a tropical island. Surely, Christmas will go on without you.

The tropical island has its own currency and is entirely cash-only. The gold coins used there have a little picture of a starfish; the locals just call them stars. None of the currency exchanges seem to have heard of them, but somehow, you'll need to find fifty of these coins by the time you arrive so you can pay the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then multiply those two numbers together.

--- Part Two ---

The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?

-}

computeMagicNumber1 :: [Int] -> Int
computeMagicNumber1 [] = 0
computeMagicNumber1 l | length pair == 1 = head $ map (\(x,y) -> x * y) pair
                      | otherwise = 0
 where
   pair = filter (\(x,y) -> x+y == 2020) [ (x,y) | x <- l, y <- l, x < y ]

readInt :: [String] -> [Int]
readInt = map read

solvePart1 :: IO ()
solvePart1 = putStr . show . computeMagicNumber1 . readInt . words =<< readFile "inputs/day1.txt"

computeMagicNumber2 :: [Int] -> Int
computeMagicNumber2 [] = 0
computeMagicNumber2 l | length triplet == 1 = head $ map (\(x,y,z) -> x * y * z) triplet
                      | otherwise = 0
 where
   triplet = filter (\(x,y,z) -> x+y+z == 2020) [ (x,y,z) | x <- l, y <- l, z <- l, x < y && y < z ]

solvePart2 :: IO ()
solvePart2 = putStr . show . computeMagicNumber2 . readInt . words =<< readFile "inputs/day1.txt"

solve :: IO ()
solve = do
  putStr "Part 1: "
  solvePart1
  putStr "\n"
  putStr "Part 2: "
  solvePart2
