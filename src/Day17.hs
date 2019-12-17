module Day17 where

import           Data.Char
import           Data.Foldable
import           Data.Maybe
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

type Direction = V2 Int

type Position = V2 Int

type Scaffolding = Set (V2 Int)

type Robot = (Position, Direction)

data SimpleMovement
  = F
  | L
  | R
  deriving (Show)

parseAscii :: String -> (Scaffolding, Robot)
parseAscii ascii =
  (Set.fromList $ catMaybes scaffolding, head $ catMaybes robots)
  where
    (scaffolding, robots) =
      unzip . concatMap parseLine . zip [0 ..] $ lines ascii
    parseLine (y, l) = map (\(x, c) -> parseChar (V2 x y, c)) $ zip [0 ..] l
    parseChar (pos, c) =
      case c of
        '^' -> (Just pos, Just (pos, V2 0 (-1)))
        'v' -> (Just pos, Just (pos, V2 0 1))
        '<' -> (Just pos, Just (pos, V2 (-1) 0))
        '>' -> (Just pos, Just (pos, V2 1 0))
        '#' -> (Just pos, Nothing)
        _   -> (Nothing, Nothing)

calibrate :: Scaffolding -> Int
calibrate s = sum . Set.map product $ Set.filter isIntersection s
  where
    isIntersection pos =
      all
        (\dir -> (pos + dir) `Set.member` s)
        [V2 0 (-1), V2 0 1, V2 (-1) 0, V2 1 0]

walk :: Scaffolding -> Robot -> [SimpleMovement]
walk s (rpos, rdir) =
  replicate forwardSteps F <>
  case turn of
    Just t  -> t : walk s (rpos', rotate t rdir)
    Nothing -> []
  where
    forwardSteps =
      length $ takeWhile (\f -> (rpos + fmap (f *) rdir) `Set.member` s) [1 ..]
    rpos' = rpos + fmap (forwardSteps *) rdir
    turn = find (\t -> (rpos' + rotate t rdir) `Set.member` s) [L, R]
    rotate L (V2 x y) = V2 y (-x)
    rotate R (V2 x y) = V2 (-y) x

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input17"

part1 :: IO Int
part1 = do
  p <- program
  let ascii = map chr $ Intcode.run p []
  pure . calibrate . fst $ parseAscii ascii

{-
Solution found by inspecting the output of walk (above) to find the largest
repeating patterns.

Main:
A,C,A,C,B,B,C,A,C,B
Function A:
L,4,R,8,L,6,L,10
Function B:
L,4,L,4,L,10
Function C:
L,6,R,8,R,10,L,6,L,6
-}
part2Interactive :: IO ()
part2Interactive = do
  p <- program
  go . Intcode.runDynamic $ 2 : tail p
  where
    go (Intcode.Input f)     = getChar >>= go . f . ord
    go (Intcode.Output o c') = putStr (display o) >> go c'
    go Intcode.Stop          = pure ()
    display o
      | isAscii $ chr o = [chr o]
      | otherwise = show o

part2 :: IO Int
part2 = do
  p <- program
  pure . last . Intcode.run (2 : tail p) . map ord $
    unlines
      [ "A,C,A,C,B,B,C,A,C,B"
      , "L,4,R,8,L,6,L,10"
      , "L,4,L,4,L,10"
      , "L,6,R,8,R,10,L,6,L,6"
      , "n"
      ]

testInput :: String
testInput =
  unlines
    [ "..#.........."
    , "..#.........."
    , "#######...###"
    , "#.#...#...#.#"
    , "#############"
    , "..#...#...#.."
    , "..#####...^.."
    ]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day17"
    [ testCase "calibrate" $ (calibrate . fst $ parseAscii testInput) @?= 76
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3192
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 684691
    ]
