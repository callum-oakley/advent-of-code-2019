module Day03 where

import           Data.Foldable
import           Data.List.Split
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Test.Tasty
import           Test.Tasty.HUnit

data Direction
  = R
  | U
  | L
  | D
  deriving (Show, Read)

type Step = Int

type Move = (Direction, Int)

type Path = [Move]

type Grid = Map (Int, Int) Step

parse :: String -> (Path, Path)
parse s = (a, b)
  where
    [a, b] = map (map parseMove . splitOn ",") . lines $ s
    parseMove (direction:n) = (read [direction], read n)

steps :: ((Int, Int), Step, Grid) -> Move -> ((Int, Int), Step, Grid)
steps ((x, y), step, g) (direction, n) = ((x', y'), step + n, Map.union g g')
  where
    ((x', y'), g') =
      case direction of
        R -> ((x + n, y), Map.fromList [((x + m, y), step + m) | m <- [1 .. n]])
        U -> ((x, y + n), Map.fromList [((x, y + m), step + m) | m <- [1 .. n]])
        L -> ((x - n, y), Map.fromList [((x - m, y), step + m) | m <- [1 .. n]])
        D -> ((x, y - n), Map.fromList [((x, y - m), step + m) | m <- [1 .. n]])

walk :: Path -> Grid
walk p = g
  where
    (_, _, g) = foldl steps ((0, 0), 0, mempty) p

part1' :: (Path, Path) -> Int
part1' (p, q) =
  minimum . map manhattan . Map.keys $ Map.intersection (walk p) (walk q)
  where
    manhattan (x, y) = abs x + abs y

part2' :: (Path, Path) -> Int
part2' (p, q) = minimum $ Map.intersectionWith (+) (walk p) (walk q)

paths :: IO (Path, Path)
paths = parse <$> readFile "data/input03"

part1 :: IO Int
part1 = part1' <$> paths

part2 :: IO Int
part2 = part2' <$> paths

test =
  defaultMain $
  testGroup
    "day03"
    [ testCase "part1'" $ do
        part1' (parse $ unlines ["R8,U5,L5,D3", "U7,R6,D4,L4"]) @?= 6
        part1'
          (parse $
           unlines
             [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
             , "U62,R66,U55,R34,D71,R55,D58,R83"
             ]) @?=
          159
        part1'
          (parse $
           unlines
             [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
             , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
             ]) @?=
          135
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 5319
    , testCase "part2'" $ do
        part2' (parse $ unlines ["R8,U5,L5,D3", "U7,R6,D4,L4"]) @?= 30
        part2'
          (parse $
           unlines
             [ "R75,D30,R83,U83,L12,D49,R71,U7,L72"
             , "U62,R66,U55,R34,D71,R55,D58,R83"
             ]) @?=
          610
        part2'
          (parse $
           unlines
             [ "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
             , "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
             ]) @?=
          410
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 122514
    ]
