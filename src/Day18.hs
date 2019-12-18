{-# LANGUAGE RecordWildCards #-}

module Day18 where

import           Data.Char
import           Data.Map.Strict  (Map, (!))
import qualified Data.Map.Strict  as Map
import           Data.Sequence    (Seq ((:<|)), (><))
import qualified Data.Sequence    as Seq
import           Data.Set         (Set)
import qualified Data.Set         as Set
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

data Avatar =
  Avatar
    { position :: V2 Int
    , keys     :: Set Char
    , steps    :: Int
    }
  deriving (Show)

isKey :: Tile -> Bool
isKey (Key _) = True
isKey _       = False

-- Consider avatars equal for the purpose of the bredth first search if they
-- have the same representation.
represent :: Avatar -> (V2 Int, Set Char)
represent Avatar {..} = (position, keys)

findTile :: Maze -> Tile -> V2 Int
findTile m tile =
  fst . head . filter (\(_, tile') -> tile' == tile) $ Map.assocs m

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

bredthFirst :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bredthFirst rep step start = go Set.empty (Seq.singleton start)
  where
    go _ Seq.Empty = []
    go seen (x :<| xs)
      | Set.member (rep x) seen = go seen xs
      | otherwise =
        x : go (Set.insert (rep x) seen) (xs >< Seq.fromList (step x))

moves :: Maze -> Avatar -> [Avatar]
moves maze Avatar {..} =
  [ Avatar {position = position', keys = keys', steps = steps + 1}
  | dir <- [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]
  , let position' = position + dir
  , Map.member position' maze
  , let tile = maze ! position'
  , case tile of
      Door c -> Set.member c keys
      _      -> True
  , let keys' =
          case tile of
            Key c -> Set.insert c keys
            _     -> keys
  ]

part1' :: String -> Int
part1' s =
  steps . head . filter (\Avatar {..} -> Set.size keys == nKeys) $
  bredthFirst
    represent
    (moves maze)
    (Avatar {position = findTile maze Entrance, keys = Set.empty, steps = 0})
  where
    maze = parse s
    nKeys = Map.size $ Map.filter isKey maze

part1 :: IO Int
part1 = part1' <$> readFile "data/input18"

testData :: [String]
testData =
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

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day18"
    [ testCase "part1'" . sequence_ $
      zipWith
        (\s expected -> part1' s @?= expected)
        testData
        [8, 86, 132, 136, 81]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3546
    ]
