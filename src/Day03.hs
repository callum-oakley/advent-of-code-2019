module Day03 where

import           Data.List.Split
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

type Move = (Direction, Int)

type Path = [Move]

type Grid = Set (Int, Int)

parse :: String -> (Path, Path)
parse s = (a, b)
  where
    [a, b] = map (map parseMove . splitOn ",") . lines $ s
    parseMove (direction:n) = (read [direction], read n)

steps :: ((Int, Int), Grid) -> Move -> ((Int, Int), Grid)
steps ((x, y), g) (direction, n) = ((x', y'), Set.union g' g)
  where
    ((x', y'), g') =
      case direction of
        R -> ((x + n, y), Set.fromList [(x + m, y) | m <- [1 .. n]])
        U -> ((x, y + n), Set.fromList [(x, y + m) | m <- [1 .. n]])
        L -> ((x - n, y), Set.fromList [(x - m, y) | m <- [1 .. n]])
        D -> ((x, y - n), Set.fromList [(x, y - m) | m <- [1 .. n]])

walk :: Path -> Grid
walk = snd . foldl steps ((0, 0), Set.empty)

part1' :: (Path, Path) -> Int
part1' (p, q) =
  Set.findMin . Set.map manhattan $ Set.intersection (walk p) (walk q)
  where
    manhattan (x, y) = abs x + abs y

paths :: IO (Path, Path)
paths = parse <$> readFile "data/input03"

part1 :: IO Int
part1 = part1' <$> paths

test =
  defaultMain $
  testGroup
    "day03"
    [ testCase "part1'" $ do
        part1' (parse "R8,U5,L5,D3\nU7,R6,D4,L4") @?= 6
        part1'
          (parse
             "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83") @?=
          159
        part1'
          (parse
             "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7") @?=
          135
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 5319
    ]
