module Day10 where

import           Data.List
import           Data.Ord
import           Data.Ratio
import qualified Data.Set         as Set
import           Data.Sort
import           Test.Tasty
import           Test.Tasty.HUnit

type Asteroid = (Integer, Integer)

type Amplitude = Integer

data Phase
  = NE Rational
  | SE Rational
  | SW Rational
  | NW Rational
  deriving (Eq, Ord, Show)

parse :: String -> [Asteroid]
parse = concatMap parseRow . zip [0 ..] . lines
  where
    parseRow (y, row) = foldl (step y) [] . zip [0 ..] $ row
    step y acc (x, '#') = (x, y) : acc
    step _ acc _        = acc

phase :: Asteroid -> Phase
phase (x, y)
  | x >= 0 && y < 0 = NE (x % (-y))
  | x > 0 && y >= 0 = SE (y % x)
  | x <= 0 && y > 0 = SW ((-x) % y)
  | x < 0 && y <= 0 = NW ((-y) % (-x))

amplitude :: Asteroid -> Amplitude
amplitude (x, y) = x ^ 2 + y ^ 2

visibility :: [Asteroid] -> Asteroid -> Int
visibility as (x0, y0) =
  Set.size $
  Set.fromList [phase (x - x0, y - y0) | (x, y) <- as, (x, y) /= (x0, y0)]

station :: [Asteroid] -> Asteroid
station as = maximumBy (comparing $ visibility as) as

part1' :: [Asteroid] -> Int
part1' as = visibility as $ station as

vaporised :: [Asteroid] -> [Asteroid]
vaporised as =
  map (\(x, y) -> (x + x0, y + y0)) $
  order [(x - x0, y - y0) | (x, y) <- as, (x, y) /= (x0, y0)]
  where
    (x0, y0) = station as
    order =
      map snd .
      sortOn fst .
      concatMap (zipWith (\z a -> ((z, phase a), a)) [0 ..] . sortOn amplitude) .
      groupSortOn phase (\_ a as -> a : as)

part2' :: [Asteroid] -> Integer
part2' as = 100 * x + y
  where
    (x, y) = vaporised as !! 199

asteroids :: IO [Asteroid]
asteroids = parse <$> readFile "data/input10"

part1 :: IO Int
part1 = part1' <$> asteroids

part2 :: IO Integer
part2 = part2' <$> asteroids

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
    , testCase "part2'" $
      part2'
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
      802
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 296
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 204
    ]
