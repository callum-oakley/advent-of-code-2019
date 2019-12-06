module Day06 where

import           Data.List
import           Data.List.Split
import           Data.Map.Strict  (Map, (!?))
import qualified Data.Map.Strict  as Map
import           Test.Tasty
import           Test.Tasty.HUnit

type Graph = Map String String

parse :: String -> Graph
parse = Map.fromList . map (f . splitOn ")") . lines
  where
    f [x, y] = (y, x)

-- The path from any given node to the root.
path :: Graph -> String -> [String]
path g node = maybe [] (\n -> n : path g n) (g !? node)

part1' :: Graph -> Int
part1' g = sum [length $ path g node | node <- Map.keys g]

part2' :: Graph -> Int
part2' g = length $ (pathYOU \\ pathSAN) `union` (pathSAN \\ pathYOU)
  where
    pathYOU = path g "YOU"
    pathSAN = path g "SAN"

graph :: IO Graph
graph = parse <$> readFile "data/input06"

part1 :: IO Int
part1 = part1' <$> graph

part2 :: IO Int
part2 = part2' <$> graph

test =
  defaultMain $
  testGroup
    "day06"
    [ testCase "part1'" $ do
        g <- parse <$> readFile "data/test06-1"
        part1' g @?= 42
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 301100
    , testCase "part2'" $ do
        g <- parse <$> readFile "data/test06-2"
        part2' g @?= 4
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 547
    ]
