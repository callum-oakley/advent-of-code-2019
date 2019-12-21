module Day21 where

import           Data.Char
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

part1 :: IO Int
part1 = do
  p <- Intcode.parse <$> readFile "data/input21"
  pure . last . Intcode.run p . map ord $
    unlines
      ["OR A T", "AND B T", "AND C T", "NOT T T", "AND D T", "OR T J", "WALK"]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day21"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 19353692
    ]
