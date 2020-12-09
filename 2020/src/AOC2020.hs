module AOC2020 (program) where

import Options.Applicative
import Control.Monad
import qualified AOC2020.Day1 as Day1
import qualified AOC2020.Day2 as Day2
import qualified AOC2020.Day3 as Day3
import qualified AOC2020.Day4 as Day4
import qualified AOC2020.Day5 as Day5
import qualified AOC2020.Day6 as Day6
import qualified AOC2020.Day7 as Day7

opts :: Parser (IO ())
opts = subparser
  ( command "day1" (info (pure Day1.solve) idm)
 <> command "day2" (info (pure Day2.solve) idm)
 <> command "day3" (info (pure Day3.solve) idm)
 <> command "day4" (info (pure Day4.solve) idm)
 <> command "day5" (info (pure Day5.solve) idm)
 <> command "day6" (info (pure Day6.solve) idm)
 <> command "day7" (info (pure Day7.solve) idm) )

program :: IO ()
program = join $ execParser (info opts idm)
