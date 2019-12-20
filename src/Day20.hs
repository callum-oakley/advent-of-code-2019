{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Day20 where

import           Data.Char
import           Data.List
import           Data.Map.Strict  (Map, (!))
import qualified Data.Map.Strict  as Map
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Search

data Edge
  = Outer
  | Inner
  deriving (Show, Eq)

data Tile
  = Start
  | Finish
  | Portal (V2 Int) Edge
  | Empty
  deriving (Show, Eq)

type Maze = Map (V2 Int) Tile

data State =
  State
    { position :: V2 Int
    , steps    :: Int
    , level    :: Int
    }
  deriving (Show)

windows :: Int -> [a] -> [[a]]
windows n xs = take (length xs - n + 1) . map (take n) $ tails xs

parse :: String -> Maze
parse s =
  foldr
    (\[p, q] m ->
       if isOuter p
         then Map.insert p (Portal q Outer) $ Map.insert q (Portal p Inner) m
         else Map.insert p (Portal q Inner) $ Map.insert q (Portal p Outer) m)
    (Map.insert finish Finish $ Map.insert start Start empties)
    portals
  where
    chars =
      zipWith (\y l -> zipWith (\x c -> (V2 x y, c)) [0 ..] l) [0 ..] $ lines s
    empties =
      Map.fromList . map (\(pos, _) -> (pos, Empty)) . filter ((== '.') . snd) $
      concat chars
    isOuter (V2 x y) =
      x == 2 || y == 2 || x == length (head chars) - 3 || y == length chars - 3
    (Just start, Just finish, portals) =
      foldr
        (\[(p1, c1), (_, c2), (p3, c3)] (aa, zz, ps) ->
           case (c1, c2, c3) of
             ('A', 'A', '.') -> (Just p3, zz, ps)
             ('.', 'A', 'A') -> (Just p1, zz, ps)
             ('Z', 'Z', '.') -> (aa, Just p3, ps)
             ('.', 'Z', 'Z') -> (aa, Just p1, ps)
             (_, _, '.')
               | isAlpha c1 && isAlpha c2 ->
                 (aa, zz, Map.insertWith (++) [c1, c2] [p3] ps)
             ('.', _, _)
               | isAlpha c2 && isAlpha c3 ->
                 (aa, zz, Map.insertWith (++) [c2, c3] [p1] ps)
             _ -> (aa, zz, ps))
        (Nothing, Nothing, Map.empty) .
      concat $
      map (windows 3) chars <> map (windows 3) (transpose chars)

findTile :: Maze -> (Tile -> Bool) -> V2 Int
findTile maze p = fst . head . filter (\(_, tile) -> p tile) $ Map.assocs maze

shortestPath :: (Maze -> State -> [State]) -> Maze -> Int
shortestPath moves maze =
  steps .
  head .
  filter (\State {..} -> (position, level) == (finish, 0)) .
  Search.breadthFirst (\State {..} -> (position, level)) (moves maze) $
  State {position = start, steps = 0, level = 0}
  where
    start = findTile maze (== Start)
    finish = findTile maze (== Finish)

adjacent :: Maze -> State -> [State]
adjacent maze State {..} =
  map (\pos -> State {position = pos, steps = steps + 1, level}) $
  filter
    (`Map.member` maze)
    [position + dir | dir <- [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]]

moves1 :: Maze -> State -> [State]
moves1 maze state@State {..} =
  case maze ! position of
    Portal portal _ ->
      State {position = portal, steps = steps + 1, level} : adjacent maze state
    _ -> adjacent maze state

moves2 :: Maze -> State -> [State]
moves2 maze state@State {..} =
  case maze ! position of
    Portal portal Outer
      | level > 0 ->
        State {position = portal, steps = steps + 1, level = level - 1} :
        adjacent maze state
    Portal portal Inner ->
      State {position = portal, steps = steps + 1, level = level + 1} :
      adjacent maze state
    _ -> adjacent maze state

part1' :: String -> Int
part1' = shortestPath moves1 . parse

part2' :: String -> Int
part2' = shortestPath moves2 . parse

part1 :: IO Int
part1 = part1' <$> readFile "data/input20"

part2 :: IO Int
part2 = part2' <$> readFile "data/input20"

testData :: [String]
testData =
  map
    unlines
    [ [ "         A           "
      , "         A           "
      , "  #######.#########  "
      , "  #######.........#  "
      , "  #######.#######.#  "
      , "  #######.#######.#  "
      , "  #######.#######.#  "
      , "  #####  B    ###.#  "
      , "BC...##  C    ###.#  "
      , "  ##.##       ###.#  "
      , "  ##...DE  F  ###.#  "
      , "  #####    G  ###.#  "
      , "  #########.#####.#  "
      , "DE..#######...###.#  "
      , "  #.#########.###.#  "
      , "FG..#########.....#  "
      , "  ###########.#####  "
      , "             Z       "
      , "             Z       "
      ]
    , [ "                   A               "
      , "                   A               "
      , "  #################.#############  "
      , "  #.#...#...................#.#.#  "
      , "  #.#.#.###.###.###.#########.#.#  "
      , "  #.#.#.......#...#.....#.#.#...#  "
      , "  #.#########.###.#####.#.#.###.#  "
      , "  #.............#.#.....#.......#  "
      , "  ###.###########.###.#####.#.#.#  "
      , "  #.....#        A   C    #.#.#.#  "
      , "  #######        S   P    #####.#  "
      , "  #.#...#                 #......VT"
      , "  #.#.#.#                 #.#####  "
      , "  #...#.#               YN....#.#  "
      , "  #.###.#                 #####.#  "
      , "DI....#.#                 #.....#  "
      , "  #####.#                 #.###.#  "
      , "ZZ......#               QG....#..AS"
      , "  ###.###                 #######  "
      , "JO..#.#.#                 #.....#  "
      , "  #.#.#.#                 ###.#.#  "
      , "  #...#..DI             BU....#..LF"
      , "  #####.#                 #.#####  "
      , "YN......#               VT..#....QG"
      , "  #.###.#                 #.###.#  "
      , "  #.#...#                 #.....#  "
      , "  ###.###    J L     J    #.#.###  "
      , "  #.....#    O F     P    #.#...#  "
      , "  #.###.#####.#.#####.#####.###.#  "
      , "  #...#.#.#...#.....#.....#.#...#  "
      , "  #.#####.###.###.#.#.#########.#  "
      , "  #...#.#.....#...#.#.#.#.....#.#  "
      , "  #.###.#####.###.###.#.#.#######  "
      , "  #.#.........#...#.............#  "
      , "  #########.###.###.#############  "
      , "           B   J   C               "
      , "           U   P   P               "
      ]
    ]

testData2 :: String
testData2 =
  unlines
    [ "             Z L X W       C                 "
    , "             Z P Q B       K                 "
    , "  ###########.#.#.#.#######.###############  "
    , "  #...#.......#.#.......#.#.......#.#.#...#  "
    , "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
    , "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
    , "  #.###.#######.###.###.#.###.###.#.#######  "
    , "  #...#.......#.#...#...#.............#...#  "
    , "  #.#########.#######.#.#######.#######.###  "
    , "  #...#.#    F       R I       Z    #.#.#.#  "
    , "  #.###.#    D       E C       H    #.#.#.#  "
    , "  #.#...#                           #...#.#  "
    , "  #.###.#                           #.###.#  "
    , "  #.#....OA                       WB..#.#..ZH"
    , "  #.###.#                           #.#.#.#  "
    , "CJ......#                           #.....#  "
    , "  #######                           #######  "
    , "  #.#....CK                         #......IC"
    , "  #.###.#                           #.###.#  "
    , "  #.....#                           #...#.#  "
    , "  ###.###                           #.#.#.#  "
    , "XF....#.#                         RF..#.#.#  "
    , "  #####.#                           #######  "
    , "  #......CJ                       NM..#...#  "
    , "  ###.#.#                           #.###.#  "
    , "RE....#.#                           #......RF"
    , "  ###.###        X   X       L      #.#.#.#  "
    , "  #.....#        F   Q       P      #.#.#.#  "
    , "  ###.###########.###.#######.#########.###  "
    , "  #.....#...#.....#.......#...#.....#.#...#  "
    , "  #####.#.###.#######.#######.###.###.#.#.#  "
    , "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
    , "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
    , "  #.......#.....#.#...#...............#...#  "
    , "  #############.#.#.###.###################  "
    , "               A O F   N                     "
    , "               A A D   M                     "
    ]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day20"
    [ testCase "part1'" . sequence_ $
      zipWith (\s expected -> part1' s @?= expected) testData [23, 58]
    , testCase "part2'" $ part2' testData2 @?= 396
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 422
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 5040
    ]
