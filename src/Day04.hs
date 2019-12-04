module Day04 where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Test.Tasty
import           Test.Tasty.HUnit

hasPair :: String -> Bool
hasPair = any (>= 2) . map length . group

hasStandalonePair :: String -> Bool
hasStandalonePair = elem 2 . map length . group

nonDecreasing :: [String]
nonDecreasing = map (map intToDigit) $ concat [go [d] | d <- [0 .. 9]]
  where
    go p
      | length p == 6 = [p]
    go (p:ps) = concat [go (q : p : ps) | q <- [0 .. p]]

range :: (String, String)
range = ("134792", "675810")

part1 :: Int
part1 =
  length [p | p <- nonDecreasing, hasPair p, p >= fst range, p <= snd range]

part2 :: Int
part2 =
  length
    [ p
    | p <- nonDecreasing
    , hasStandalonePair p
    , p >= fst range
    , p <= snd range
    ]

test =
  defaultMain $
  testGroup
    "day04"
    [testCase "part1" $ part1 @?= 1955, testCase "part2" $ part2 @?= 1319]
