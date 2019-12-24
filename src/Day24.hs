module Day24 where

import           Data.Map.Strict  (Map, (!?))
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

data Tile
  = Bug
  | Empty
  deriving (Show, Eq)

type Eris = Map (V2 Int) Tile -- V2 y x so that elems returns a row at a time

parse :: String -> Eris
parse = Map.fromList . concatMap parseLine . zip [0 ..] . lines
  where
    parseLine (y, l) = map (parseChar y) $ zip [0 ..] l
    parseChar y (x, c) =
      ( V2 y x
      , case c of
          '#' -> Bug
          '.' -> Empty)

tick :: Eris -> Eris
tick eris = Map.mapWithKey f eris
  where
    f pos Bug
      | adjacentBugs pos == 1 = Bug
      | otherwise = Empty
    f pos Empty
      | adjacentBugs pos `elem` [1, 2] = Bug
      | otherwise = Empty
    adjacentBugs pos =
      length $
      filter
        (\dir -> eris !? (pos + dir) == Just Bug)
        [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]

biodiversity :: Eris -> Int
biodiversity = sum . zipWith f [0 ..] . Map.elems
  where
    f i Bug   = 2 ^ i
    f _ Empty = 0

part1' :: String -> Int
part1' = go Set.empty . parse
  where
    go seen eris
      | b `Set.member` seen = b
      | otherwise = go (Set.insert b seen) (tick eris)
      where
        b = biodiversity eris

part1 :: IO Int
part1 = part1' <$> readFile "data/input24"

testData :: String
testData = unlines ["....#", "#..#.", "#..##", "..#..", "#...."]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day24"
    [ testCase "part1'" $ part1' testData @?= 2129920
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 27777901
    ]
