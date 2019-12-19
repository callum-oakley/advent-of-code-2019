{-# LANGUAGE RecordWildCards #-}

module Day18 where

import           Data.Char
import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Sequence    (Seq ((:<|)), (><))
import qualified Data.Sequence    as Seq
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Vector      (Vector, (//))
import qualified Data.Vector      as Vector
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

data Tile
  = Empty
  | Entrance
  | Door Char
  | Key Char
  deriving (Show, Eq)

type Maze = Map (V2 Int) Tile

data State =
  State
    { robots :: Vector (V2 Int)
    , keys   :: Set Char
    , steps  :: Int
    , moving :: Maybe Int
    }
  deriving (Show)

isKey :: Tile -> Bool
isKey (Key _) = True
isKey _       = False

findTiles :: Maze -> (Tile -> Bool) -> [V2 Int]
findTiles m p = map fst . filter (\(_, tile) -> p tile) $ Map.assocs m

parse :: String -> Maze
parse = Map.fromList . concatMap parseLine . zip [0 ..] . lines
  where
    parseLine (y, l) =
      map (\(x, c) -> (V2 x y, parseChar c)) . filter (\(_, c) -> c /= '#') $
      zip [0 ..] l
    parseChar '.' = Empty
    parseChar '@' = Entrance
    parseChar c
      | isLower c = Key c
      | isUpper c = Door $ toLower c

breadthFirst :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
breadthFirst rep reachable start =
  go (Set.singleton $ rep start) (Seq.singleton start)
  where
    go _ Seq.Empty = []
    go seen (x :<| xs) =
      x :
      go (Set.union (Set.fromList $ map rep ys) seen) (xs >< Seq.fromList ys)
      where
        ys = filter (\y -> Set.notMember (rep y) seen) $ reachable x

moves :: Maze -> State -> [State]
moves maze State {..} =
  [ State
    { robots = robots // [(i, robot')]
    , keys = keys'
    , steps = steps + 1
    , moving = moving'
    }
  | (i, robot) <-
      case moving of
        Just i  -> pure (i, robots Vector.! i)
        Nothing -> toList $ Vector.indexed robots
  , dir <- [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
  , let robot' = robot + dir
  , Map.member robot' maze
  , let tile = maze Map.! robot'
  , case tile of
      Door key -> Set.member key keys
      _        -> True
  , let (keys', moving') =
          case tile of
            Key key ->
              if Set.member key keys
                then (keys, Just i)
                else (Set.insert key keys, Nothing)
            _ -> (keys, Just i)
  ]

fewestSteps :: Maze -> Int
fewestSteps maze =
  steps . head . filter (\State {..} -> Set.size keys == nKeys) $
  breadthFirst
    (\State {..} -> (robots, keys))
    (moves maze)
    (State
       { robots = Vector.fromList $ findTiles maze (== Entrance)
       , keys = Set.empty
       , steps = 0
       , moving = Nothing
       })
  where
    nKeys = Map.size $ Map.filter isKey maze

part1' :: String -> Int
part1' = fewestSteps . parse

part2' :: String -> Int
part2' s = fewestSteps $ Map.mapMaybeWithKey f maze
  where
    maze = parse s
    entrance = head $ findTiles maze (== Entrance)
    f pos tile
      | sum (fmap abs $ pos - entrance) <= 1 = Nothing
      | fmap abs (pos - entrance) == V2 1 1 = Just Entrance
      | otherwise = Just tile

part1 :: IO Int
part1 = part1' <$> readFile "data/input18"

-- This gets the right answer but it takes a pretty long time...
part2 :: IO Int
part2 = part2' <$> readFile "data/input18"

testData1 :: [String]
testData1 =
  map
    unlines
    [ ["#########", "#b.A.@.a#", "#########"]
    , [ "########################"
      , "#f.D.E.e.C.b.A.@.a.B.c.#"
      , "######################.#"
      , "#d.....................#"
      , "########################"
      ]
    , [ "########################"
      , "#...............b.C.D.f#"
      , "#.######################"
      , "#.....@.a.B.c.d.A.e.F.g#"
      , "########################"
      ]
    , [ "#################"
      , "#i.G..c...e..H.p#"
      , "########.########"
      , "#j.A..b...f..D.o#"
      , "########@########"
      , "#k.E..a...g..B.n#"
      , "########.########"
      , "#l.F..d...h..C.m#"
      , "#################"
      ]
    , [ "########################"
      , "#@..............ac.GI.b#"
      , "###d#e#f################"
      , "###A#B#C################"
      , "###g#h#i################"
      , "########################"
      ]
    ]

testData2 :: [String]
testData2 =
  map
    unlines
    [ [ "#######"
      , "#a.#Cd#"
      , "##...##"
      , "##.@.##"
      , "##...##"
      , "#cB#Ab#"
      , "#######"
      ]
    , [ "###############"
      , "#d.ABC.#.....a#"
      , "######...######"
      , "######.@.######"
      , "######...######"
      , "#b.....#.....c#"
      , "###############"
      ]
    , [ "#############"
      , "#DcBa.#.GhKl#"
      , "#.###...#I###"
      , "#e#d#.@.#j#k#"
      , "###C#...###J#"
      , "#fEbA.#.FgHi#"
      , "#############"
      ]
    , [ "#############"
      , "#g#f.D#..h#l#"
      , "#F###e#E###.#"
      , "#dCba...BcIJ#"
      , "#####.@.#####"
      , "#nK.L...G...#"
      , "#M###N#H###.#"
      , "#o#m..#i#jk.#"
      , "#############"
      ]
    ]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day18"
    [ testCase "part1'" . sequence_ $
      zipWith
        (\s expected -> part1' s @?= expected)
        testData1
        [8, 86, 132, 136, 81]
    , testCase "part2'" . sequence_ $
      zipWith (\s expected -> part2' s @?= expected) testData2 [8, 24, 32, 72]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3546
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 1988
    ]
