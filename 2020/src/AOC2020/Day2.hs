{-# LANGUAGE OverloadedStrings #-}
module AOC2020.Day2 ( countValidPasswords
                    , parsePasswordPolicy
                    , parsePasswordEntry
                    , isValidPassword1
                    , isValidPassword2
                    , solve
                    , PasswordPolicy (..)
                    , Password (..)
                    ) where

import Data.List.Split
import qualified Data.Text as T
import Data.List (findIndices)

{-

--- Day 2: Password Philosophy ---

Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.

Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

How many passwords are valid according to their policies?

--- Part Two ---

While it appears you validated the passwords correctly, they don't seem to be what the Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.

Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.

-}

type MinCharCount = Int
type MaxCharCount = Int
type RequiredChar = Char
data PasswordPolicy = PasswordPolicy MinCharCount MaxCharCount RequiredChar
                    deriving (Show, Eq)
data Password = Password String
              deriving (Show, Eq)

trim :: String -> String
trim = T.unpack . T.strip . T.pack

readInt :: String -> Int
readInt = read

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

parsePasswordPolicy :: String -> PasswordPolicy
parsePasswordPolicy str = PasswordPolicy minCount maxCount (head requiredChar)
 where
   [countRange, requiredChar] = map trim $ splitOn " " str
   [minCount, maxCount] = map (readInt . trim) $ splitOn "-" countRange

parsePasswordEntry :: String -> (PasswordPolicy, Password)
parsePasswordEntry str = (passwordPolicy, (Password passStr))
 where
   [policyStr, passStr] = map trim $ splitOn ":" str
   passwordPolicy = parsePasswordPolicy policyStr

isValidPassword1 :: (PasswordPolicy, Password) -> Bool
isValidPassword1 (PasswordPolicy minCount maxCount requiredChar, Password pass) = requiredCharCount >= minCount && requiredCharCount <= maxCount
 where
   requiredCharCount = count requiredChar pass

countValidPasswords :: ((PasswordPolicy, Password) -> Bool) -> [String] -> Int
countValidPasswords _ [] = 0
countValidPasswords policyCheck passEntries = count True validPasswords
 where
   validPasswords = map (policyCheck . parsePasswordEntry) passEntries

solvePart1 :: IO ()
solvePart1 = putStr . show . (countValidPasswords isValidPassword1) . lines =<< readFile "inputs/day2.txt"

isValidPassword2 :: (PasswordPolicy, Password) -> Bool
isValidPassword2 (PasswordPolicy firstPos secondPos requiredChar, Password pass) = firstPosHasRequiredChar `xor` secondPosHasRequiredChar
 where
   requiredCharIndices = findIndices (==requiredChar) pass
   firstPosHasRequiredChar = (firstPos - 1) `elem` requiredCharIndices
   secondPosHasRequiredChar = (secondPos - 1) `elem` requiredCharIndices

solvePart2 :: IO ()
solvePart2 = putStr . show . (countValidPasswords isValidPassword2) . lines =<< readFile "inputs/day2.txt"

solve :: IO ()
solve = do
  putStr "Part 1: "
  solvePart1
  putStr "\n"
  putStr "Part 2: "
  solvePart2
