{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Day01 where

import           Test.Framework

parse :: String -> [Integer]
parse = map read . lines

fuel :: Integer -> Integer
fuel mass = mass `div` 3 - 2

fuel2 :: Integer -> Integer
fuel2 = sum . takeWhile (> 0) . tail . iterate fuel

masses :: IO [Integer]
masses = parse <$> readFile "day01/input"

part1 :: IO Integer
part1 = sum . map fuel <$> masses

part2 :: IO Integer
part2 = sum . map fuel2 <$> masses

test_fuel =
  mapM
    (\(s, t) -> assertEqual t (fuel s))
    [(12, 2), (14, 2), (1969, 654), (100756, 33583)]

test_fuel2 =
  mapM
    (\(s, t) -> assertEqual t (fuel2 s))
    [(14, 2), (1969, 966), (100756, 50346)]

test = htfMain htf_thisModulesTests
