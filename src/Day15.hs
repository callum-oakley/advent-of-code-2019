{-# LANGUAGE TupleSections #-}

module Day15 where

import           Data.Map.Strict  (Map, (!))
import qualified Data.Map.Strict  as Map
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

data Tile
  = Origin
  | Empty
  | Oxygen
  | Wall
  deriving (Show, Eq)

type Seconds = Int

hugWall :: Intcode.Effect -> V2 Int -> V2 Int -> Map (V2 Int) Tile
hugWall computer' pos' dir' =
  go computer' pos' dir' (Map.singleton pos' Origin) True
  where
    go computer pos dir m first
      | (pos, dir, first) == (pos', dir', False) = m
      | otherwise =
        case Intcode.expectInput computer $ encodeInput dir of
          (Intcode.Output 0 c) -> go c pos (turnR dir) m False
          (Intcode.Output 1 c) ->
            go c (pos + dir) (turnL dir) (insert (pos + dir) Empty m) False
          (Intcode.Output 2 c) ->
            go c (pos + dir) (turnL dir) (insert (pos + dir) Oxygen m) False
    insert = Map.insertWith (flip const)
    turnR (V2 x y) = V2 (-y) x
    turnL (V2 x y) = V2 y (-x)
    encodeInput (V2 0 (-1)) = 1
    encodeInput (V2 0 1)    = 2
    encodeInput (V2 (-1) 0) = 3
    encodeInput (V2 1 0)    = 4

explore :: Intcode.Program -> Map (V2 Int) Tile
explore p = hugWall (Intcode.runDynamic p) 0 (V2 0 (-1))

findTile :: Map (V2 Int) Tile -> Tile -> V2 Int
findTile m tile =
  fst . head . filter (\(_, tile') -> tile' == tile) $ Map.assocs m

flood :: Map (V2 Int) Tile -> Map (V2 Int) Seconds
flood m = go (Map.singleton (findTile m Oxygen) 0) 1
  where
    go ts t =
      case boundary ts of
        [] -> ts
        bs -> go (Map.union (Map.fromList $ map (, t) bs) ts) (t + 1)
    boundary ts =
      filter
        (\pos -> Map.notMember pos ts && any (`Map.member` ts) (adjacent pos)) $
      Map.keys m
    adjacent (V2 x y) = [V2 (x + 1) y, V2 x (y + 1), V2 (x - 1) y, V2 x (y - 1)]

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input15"

part1 :: IO Int
part1 = do
  m <- explore <$> program
  return $ flood m ! findTile m Origin

part2 :: IO Int
part2 = maximum . flood . explore <$> program

plot :: Map (V2 Int) Tile -> IO ()
plot m = do
  let xmin = minimum . map (\(V2 x _) -> x - 1) $ Map.keys m
  let xmax = maximum . map (\(V2 x _) -> x + 1) $ Map.keys m
  let ymin = minimum . map (\(V2 _ y) -> y - 1) $ Map.keys m
  let ymax = maximum . map (\(V2 _ y) -> y + 1) $ Map.keys m
  putStrLn $
    unlines
      [ plotRow [Map.findWithDefault Wall (V2 x y) m | x <- [xmin .. xmax]]
      | y <- [ymin .. ymax]
      ]
  where
    plotRow = concatMap plotTile
    plotTile Origin = " O "
    plotTile Empty  = " · "
    plotTile Wall   = "███"
    plotTile Oxygen = " X "

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day15"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 240
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 322
    ]
