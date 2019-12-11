module Day11 where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict          (Map)
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
        0 -> V2 (-y) x
        1 -> V2 y (-x)

draw :: ReaderT (TQueue Int, TQueue Int, Async ()) (StateT (Hull, Robot) IO) ()
draw = do
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
      draw

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input11"

part1 :: IO Int
part1 = do
  p <- program
  inQ <- newTQueueIO
  outQ <- newTQueueIO
  brain <- async $ Intcode.runDynamic p (inQ, outQ)
  (hull, _) <-
    execStateT (runReaderT draw (inQ, outQ, brain)) (Map.empty, (0, V2 0 (-1)))
  return $ Map.size hull

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day11"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 2428
    ]
