module Day24 where

import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Linear.V3
import           Test.Tasty
import           Test.Tasty.HUnit

type Eris = Set (V3 Int)

parse :: String -> Eris
parse = Set.fromList . concatMap parseLine . zip [0 ..] . lines
  where
    parseLine (y, l) =
      map fst . filter ((== '#') . snd) . map (parseChar y) $ zip [0 ..] l
    parseChar y (x, c) = (V3 x y 0, c)

tick1 :: Eris -> Eris
tick1 eris = Set.fromList $ filter isBug positions
  where
    isBug pos
      | pos `Set.member` eris = adjacentBugs pos == 1
      | otherwise = adjacentBugs pos `elem` [1, 2]
    adjacentBugs = length . filter (`Set.member` eris) . adjacent
    positions = [V3 x y 0 | x <- [0 .. 4], y <- [0 .. 4]]
    adjacent pos =
      filter
        (\(V3 x y _) -> x >= 0 && x <= 4 && y >= 0 && y <= 4)
        [pos + dir | dir <- [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0]]

tick2 :: Eris -> Eris
tick2 eris = Set.fromList $ filter isBug positions
  where
    isBug pos
      | pos `Set.member` eris = adjacentBugs pos == 1
      | otherwise = adjacentBugs pos `elem` [1, 2]
    adjacentBugs = length . filter (`Set.member` eris) . adjacent
    positions =
      [ V3 x y z
      | x <- [0 .. 4]
      , y <- [0 .. 4]
      , (x, y) /= (2, 2)
      , z <- [minimum zs - 1 .. maximum zs + 1]
      ]
    zs = map (\(V3 _ _ z) -> z) $ Set.toList eris
    adjacent pos =
      (case pos of
         V3 1 2 z -> [V3 0 y (z + 1) | y <- [0 .. 4]]
         V3 3 2 z -> [V3 4 y (z + 1) | y <- [0 .. 4]]
         V3 2 1 z -> [V3 x 0 (z + 1) | x <- [0 .. 4]]
         V3 2 3 z -> [V3 x 4 (z + 1) | x <- [0 .. 4]]
         V3 0 0 z -> [V3 1 2 (z - 1), V3 2 1 (z - 1)]
         V3 4 0 z -> [V3 2 1 (z - 1), V3 3 2 (z - 1)]
         V3 4 4 z -> [V3 3 2 (z - 1), V3 2 3 (z - 1)]
         V3 0 4 z -> [V3 2 3 (z - 1), V3 1 2 (z - 1)]
         V3 0 _ z -> [V3 1 2 (z - 1)]
         V3 4 _ z -> [V3 3 2 (z - 1)]
         V3 _ 0 z -> [V3 2 1 (z - 1)]
         V3 _ 4 z -> [V3 2 3 (z - 1)]
         _        -> []) <>
      filter
        (\(V3 x y _) -> x >= 0 && x <= 4 && y >= 0 && y <= 4 && (x, y) /= (2, 2))
        [pos + dir | dir <- [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0]]

biodiversity :: Eris -> Int
biodiversity = sum . Set.map (\(V3 x y _) -> 2 ^ (x + 5 * y))

part1' :: String -> Int
part1' = go Set.empty . parse
  where
    go seen eris
      | b `Set.member` seen = b
      | otherwise = go (Set.insert b seen) (tick1 eris)
      where
        b = biodiversity eris

part2' :: Int -> String -> Int
part2' i = Set.size . (!! i) . iterate tick2 . parse

part1 :: IO Int
part1 = part1' <$> readFile "data/input24"

part2 :: IO Int
part2 = part2' 200 <$> readFile "data/input24"

testData :: String
testData = unlines ["....#", "#..#.", "#..##", "..#..", "#...."]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day24"
    [ testCase "part1'" $ part1' testData @?= 2129920
    , testCase "part2'" $ part2' 10 testData @?= 99
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 27777901
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 2047
    ]
