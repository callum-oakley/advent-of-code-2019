module Day01 where

import           Test.Tasty
import           Test.Tasty.HUnit

parse :: String -> [Integer]
parse = map read . lines

fuel :: Integer -> Integer
fuel mass = mass `div` 3 - 2

fuel2 :: Integer -> Integer
fuel2 = sum . takeWhile (> 0) . tail . iterate fuel

masses :: IO [Integer]
masses = parse <$> readFile "data/input01"

part1 :: IO Integer
part1 = sum . map fuel <$> masses

part2 :: IO Integer
part2 = sum . map fuel2 <$> masses

test =
  defaultMain $
  testGroup
    "day01"
    [ testCase "fuel" $ do
        fuel 12 @?= 2
        fuel 14 @?= 2
        fuel 1969 @?= 654
        fuel 100756 @?= 33583
    , testCase "fuel2" $ do
        fuel2 14 @?= 2
        fuel2 1969 @?= 966
        fuel2 100756 @?= 50346
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3252897
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 4876469
    ]
