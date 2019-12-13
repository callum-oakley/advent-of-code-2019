module Day11 where

import           Data.Map.Strict  (Map, (!))
import qualified Data.Map.Strict  as Map
import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

type Hull = Map (V2 Int) Int

type Position = V2 Int

type Direction = V2 Int

type Robot = (Position, Direction)

move :: Int -> Robot -> Robot
move turn (position, V2 x y) = (position + direction, direction)
  where
    direction =
      case turn of
        0 -> V2 y (-x)
        1 -> V2 (-y) x

paintingStates :: (Robot, Hull) -> [Int] -> [(Robot, Hull)]
paintingStates (robot@(position, _), hull) instructions =
  (robot, hull) :
  case instructions of
    (colour:turn:rest) ->
      paintingStates (move turn robot, Map.insert position colour hull) rest
    [] -> []

paint' :: Intcode.Program -> Hull -> Hull
paint' p hull = snd $ last states
  where
    states =
      paintingStates ((0, V2 0 (-1)), hull) . Intcode.run p $ map colour states
    colour ((position, _), h) = Map.findWithDefault 0 position h

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input11"

paint :: Hull -> IO Hull
paint hull = do
  p <- program
  return $ paint' p hull

part1 :: IO Int
part1 = Map.size <$> paint Map.empty

part2' :: IO String
part2' = do
  hull <- paint (Map.singleton 0 1)
  let blackSquares = filter (\pos -> hull ! pos == 1) $ Map.keys hull
  let xmin = minimum . map (\(V2 x _) -> x) $ blackSquares
  let xmax = maximum . map (\(V2 x _) -> x) $ blackSquares
  let ymin = minimum . map (\(V2 _ y) -> y) $ blackSquares
  let ymax = maximum . map (\(V2 _ y) -> y) $ blackSquares
  return $
    unlines
      [ printRow [Map.findWithDefault 0 (V2 x y) hull | x <- [xmin .. xmax]]
      | y <- [ymin .. ymax]
      ]
  where
    printRow = concatMap draw
    draw 0 = "  "
    draw 1 = "██"

part2 :: IO ()
part2 = putStrLn =<< part2'

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day11"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 2428
    , testCase "part2'" $ do
        p2 <- part2'
        p2 @?=
          unlines
            [ "██████        ████  ██        ████████  ██████    ██    ██    ████    ██    ██"
            , "██    ██        ██  ██        ██        ██    ██  ██    ██  ██    ██  ██    ██"
            , "██    ██        ██  ██        ██████    ██████    ██    ██  ██        ██    ██"
            , "██████          ██  ██        ██        ██    ██  ██    ██  ██        ██    ██"
            , "██  ██    ██    ██  ██        ██        ██    ██  ██    ██  ██    ██  ██    ██"
            , "██    ██    ████    ████████  ██        ██████      ████      ████      ████  "
            ]
    ]
