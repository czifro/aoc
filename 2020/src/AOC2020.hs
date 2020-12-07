module AOC2020 (program) where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)
import qualified AOC2020.Day1 as Day1
import qualified AOC2020.Day2 as Day2
import qualified AOC2020.Day3 as Day3
import qualified AOC2020.Day4 as Day4

opts :: Parser (IO ())
opts = subparser
  ( command "day1" (info (pure Day1.solve) idm)
 <> command "day2" (info (pure Day2.solve) idm)
 <> command "day3" (info (pure Day3.solve) idm)
 <> command "day4" (info (pure Day4.solve) idm) )

program :: IO ()
program = join $ execParser (info opts idm)
