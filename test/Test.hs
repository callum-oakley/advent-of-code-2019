import           Test.Tasty

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23

main :: IO ()
main =
  defaultMain $
  testGroup
    "main"
    [ Day01.tests
    , Day02.tests
    , Day03.tests
    , Day04.tests
    , Day05.tests
    , Day06.tests
    , Day07.tests
    , Day08.tests
    , Day09.tests
    , Day10.tests
    , Day11.tests
    , Day12.tests
    , Day13.tests
    , Day14.tests
    , Day15.tests
    , Day16.tests
    , Day17.tests
    , Day18.tests
    , Day19.tests
    , Day20.tests
    , Day21.tests
    , Day22.tests
    , Day23.tests
    ]
