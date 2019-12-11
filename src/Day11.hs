module Day11 where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict          (Map, (!))
import qualified Data.Map.Strict          as Map
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

paint :: ReaderT (TQueue Int, TQueue Int, Async ()) (StateT (Hull, Robot) IO) ()
paint = do
  (inQ, outQ, brain) <- ask
  done <- lift . lift $ poll brain
  case done of
    Just _ -> return ()
    Nothing -> do
      (hull, robot@(position, _)) <- get
      lift . lift . atomically $
        writeTQueue inQ (Map.findWithDefault 0 position hull)
      colour <- lift . lift . atomically $ readTQueue outQ
      turn <- lift . lift . atomically $ readTQueue outQ
      put (Map.insert position colour hull, move turn robot)
      paint

runPaint :: Hull -> IO Hull
runPaint hull = do
  p <- program
  inQ <- newTQueueIO
  outQ <- newTQueueIO
  brain <- async $ Intcode.runDynamic p (inQ, outQ)
  fst <$>
    execStateT (runReaderT paint (inQ, outQ, brain)) (hull, (0, V2 0 (-1)))

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input11"

part1 :: IO Int
part1 = Map.size <$> runPaint Map.empty

part2' :: IO String
part2' = do
  hull <- runPaint (Map.singleton 0 1)
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
