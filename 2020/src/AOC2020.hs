module AOC2020 (program) where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (join)
import qualified AOC2020.Day1 as Day1
import qualified AOC2020.Day2 as Day2

opts :: Parser (IO ())
opts = subparser
  ( command "day1" (info (pure Day1.solve) idm)
 <> command "day2" (info (pure Day2.solve) idm) )

program :: IO ()
program = join $ execParser (info opts idm)
