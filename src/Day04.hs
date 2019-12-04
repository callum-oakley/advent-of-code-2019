module Day04 where

import           Data.List.Split
import           Test.Tasty
import           Test.Tasty.HUnit

parse :: String -> (Int, Int)
parse s = (a, b)
  where
    [a, b] = map read . splitOn "-" $ s

hasPair :: String -> Bool
hasPair xs
  | length xs < 2 = False
hasPair (x:y:ys) = x == y || hasPair (y : ys)

hasStandalonePair :: String -> Bool
hasStandalonePair xs
  | length xs < 2 = False
hasStandalonePair [w, x] = w == x
hasStandalonePair (w:x:y:ys) = w == x && x /= y || go (w : x : y : ys)
  where
    go ws
      | length ws < 4 = False
    go [w, x, y, z] = w /= x && x == y && y /= z || x /= y && y == z
    go (w:x:y:z:zs) = w /= x && x == y && y /= z || go (x : y : z : zs)

nonDecreasing :: String -> Bool
nonDecreasing xs
  | length xs < 2 = True
nonDecreasing (x:y:ys) = x <= y && nonDecreasing (y : ys)

range :: (Int, Int)
range = parse "134792-675810"

part1 :: Int
part1 =
  length [n | n <- [lower .. upper], let m = show n, hasPair m, nonDecreasing m]
  where
    (lower, upper) = range

part2 :: Int
part2 =
  length
    [ n
    | n <- [lower .. upper]
    , let m = show n
    , hasStandalonePair m
    , nonDecreasing m
    ]
  where
    (lower, upper) = range

test =
  defaultMain $
  testGroup
    "day04"
    [testCase "part1" $ part1 @?= 1955, testCase "part2" $ part2 @?= 1319]
