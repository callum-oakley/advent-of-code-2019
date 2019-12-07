import           Test.Tasty

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07

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
    ]
