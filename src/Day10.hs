module Day10 where

import           Data.Ratio
import qualified Data.Set         as Set
import           Test.Tasty
import           Test.Tasty.HUnit

type Asteroid = (Integer, Integer)

data Ray
  = East
  | West
  | NorthIsh Rational
  | SouthIsh Rational
  deriving (Eq, Ord)

parse :: String -> [Asteroid]
parse = concatMap parseRow . zip [0 ..] . lines
  where
    parseRow (y, row) = foldl (step y) [] . zip [0 ..] $ row
    step y acc (x, '#') = (x, y) : acc
    step _ acc _        = acc

ray :: Asteroid -> Ray
ray (x, y)
  | x > 0 && y == 0 = East
  | x < 0 && y == 0 = West
  | y >= 0 = NorthIsh (x % y)
  | y <= 0 = SouthIsh (x % y)

part1' :: [Asteroid] -> Int
part1' as = maximum . map visibility $ as
  where
    visibility (x0, y0) =
      Set.size $
      Set.fromList [ray (x - x0, y - y0) | (x, y) <- as, (x, y) /= (x0, y0)]

asteroids :: IO [Asteroid]
asteroids = parse <$> readFile "data/input10"

part1 :: IO Int
part1 = part1' <$> asteroids

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day10"
    [ testCase "part1'" $ do
        part1' (parse $ unlines [".#..#", ".....", "#####", "....#", "...##"]) @?=
          8
        part1'
          (parse $
           unlines
             [ "......#.#."
             , "#..#.#...."
             , "..#######."
             , ".#.#.###.."
             , ".#..#....."
             , "..#....#.#"
             , "#..#....#."
             , ".##.#..###"
             , "##...#..#."
             , ".#....####"
             ]) @?=
          33
        part1'
          (parse $
           unlines
             [ "#.#...#.#."
             , ".###....#."
             , ".#....#..."
             , "##.#.#.#.#"
             , "....#.#.#."
             , ".##..###.#"
             , "..#...##.."
             , "..##....##"
             , "......#..."
             , ".####.###."
             ]) @?=
          35
        part1'
          (parse $
           unlines
             [ ".#..#..###"
             , "####.###.#"
             , "....###.#."
             , "..###.##.#"
             , "##.##.#.#."
             , "....###..#"
             , "..#.#..#.#"
             , "#..#.#.###"
             , ".##...##.#"
             , ".....#.#.."
             ]) @?=
          41
        part1'
          (parse $
           unlines
             [ ".#..##.###...#######"
             , "##.############..##."
             , ".#.######.########.#"
             , ".###.#######.####.#."
             , "#####.##.#.##.###.##"
             , "..#####..#.#########"
             , "####################"
             , "#.####....###.#.#.##"
             , "##.#################"
             , "#####.##.###..####.."
             , "..######..##.#######"
             , "####.##.####...##..#"
             , ".#####..#.######.###"
             , "##...#.##########..."
             , "#.##########.#######"
             , ".####.#.###.###.#.##"
             , "....##.##.###..#####"
             , ".#.#.###########.###"
             , "#.#.#.#####.####.###"
             , "###.##.####.##.#..##"
             ]) @?=
          210
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 296
    ]
