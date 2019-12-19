{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Day18 where

import           Data.Char
import           Data.Foldable
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe
import           Data.Sequence    (Seq ((:<|)), (><))
import qualified Data.Sequence    as Seq
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Vector      (Vector, (//))
import qualified Data.Vector      as Vector
import           Debug.Trace
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

data Robot =
  Robot
    { position :: V2 Int
    , steps    :: Int
    }
  deriving (Show)

data State =
  State
    { robots   :: Vector (Robot)
    , keys     :: Set Char
    , lastMove :: Maybe Robot
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

breadthFirst :: (State -> [State]) -> State -> [State]
breadthFirst step start = go Map.empty (Seq.singleton start)
  where
    go _ Seq.Empty = []
    go seen (x :<| xs) =
      x :
      go
        (Map.union
           (Map.fromList $
            map
              (\State {..} ->
                 ( (position $ fromJust lastMove, keys)
                 , steps $ fromJust lastMove))
              ys)
           seen)
        (xs >< Seq.fromList ys)
      where
        ys =
          filter
            (\State {..} ->
               isJust lastMove &&
               (Map.notMember (position $ fromJust lastMove, keys) seen ||
                seen Map.! (position $ fromJust lastMove, keys) >
                steps (fromJust lastMove))) $
          step x

moves :: Maze -> State -> [State]
moves maze State {..} =
  [ State
    {robots = robots // [(i, robot')], keys = keys', lastMove = Just robot'}
  | (i, robot) <- toList $ Vector.indexed robots
  , dir <- [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
  , let robot' =
          Robot {position = position robot + dir, steps = steps robot + 1}
  , Map.member (position robot') maze
  , let tile = maze Map.! (position robot')
  , case tile of
      Door key -> Set.member key keys
      _        -> True
  , let keys' =
          case tile of
            Key key -> Set.insert key keys
            _       -> keys
  ]

fewestSteps :: Maze -> Int
fewestSteps maze =
  sum .
  fmap steps . robots . head . filter (\State {..} -> Set.size keys == nKeys) $
  breadthFirst
    (moves maze)
    (State
       { robots =
           Vector.fromList . map (\position -> Robot {position, steps = 0}) $
           findTiles maze (== Entrance)
       , keys = Set.empty
       , lastMove = Nothing
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
    ]
