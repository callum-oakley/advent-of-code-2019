module Intcode
  ( parse
  , runDynamic
  , run
  , Program
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import           Data.List.Split
import           Data.Vector            (Vector, fromList, (!), (//))

type Program = [Int]

type Memory = Vector Int

type RelativeBase = Int

type Machine = (Int, RelativeBase, Memory)

data Mode
  = Position
  | Immediate
  | Relative
  deriving (Show, Eq)

data Parameter =
  Parameter Mode Int
  deriving (Show, Eq)

data Instruction
  = Add Parameter Parameter Parameter
  | Multiply Parameter Parameter Parameter
  | Receive Parameter
  | Send Parameter
  | JumpIfTrue Parameter Parameter
  | JumpIfFalse Parameter Parameter
  | LessThan Parameter Parameter Parameter
  | Equal Parameter Parameter Parameter
  | AdjustRelativeBase Parameter
  | Halt
  deriving (Show, Eq)

parse :: String -> Program
parse = map read . splitOn ","

instruction :: Int -> Int -> Instruction
instruction i n =
  case n `mod` 100 of
    1  -> Add (param n 1) (param n 2) (param n 3)
    2  -> Multiply (param n 1) (param n 2) (param n 3)
    3  -> Receive (param n 1)
    4  -> Send (param n 1)
    5  -> JumpIfTrue (param n 1) (param n 2)
    6  -> JumpIfFalse (param n 1) (param n 2)
    7  -> LessThan (param n 1) (param n 2) (param n 3)
    8  -> Equal (param n 1) (param n 2) (param n 3)
    9  -> AdjustRelativeBase (param n 1)
    99 -> Halt
  where
    param n m =
      case n `div` (10 ^ (m + 1)) `mod` 10 of
        0 -> Parameter Position (i + m)
        1 -> Parameter Immediate (i + m)
        2 -> Parameter Relative (i + m)

fetchVal :: Parameter -> RelativeBase -> Memory -> Int
fetchVal p b m = m ! fetchAddr p b m

fetchAddr :: Parameter -> RelativeBase -> Memory -> Int
fetchAddr (Parameter Position i) _ m  = m ! i
fetchAddr (Parameter Immediate i) _ _ = i
fetchAddr (Parameter Relative i) b m  = b + m ! i

run' :: ReaderT (TQueue Int, TQueue Int) (StateT Machine IO) ()
run' = do
  (i, b, m) <- get
  case instruction i $ m ! i of
    Add p1 p2 p3 -> do
      put
        (i + 4, b, m // [(fetchAddr p3 b m, fetchVal p1 b m + fetchVal p2 b m)])
      run'
    Multiply p1 p2 p3 -> do
      put
        (i + 4, b, m // [(fetchAddr p3 b m, fetchVal p1 b m * fetchVal p2 b m)])
      run'
    Receive p1 -> do
      inQ <- asks fst
      input <- lift . lift . atomically . readTQueue $ inQ
      put (i + 2, b, m // [(fetchAddr p1 b m, input)])
      run'
    Send p1 -> do
      outQ <- asks snd
      lift . lift . atomically . writeTQueue outQ $ fetchVal p1 b m
      put (i + 2, b, m)
      run'
    JumpIfTrue p1 p2 -> do
      put
        ( if fetchVal p1 b m /= 0
            then fetchVal p2 b m
            else i + 3
        , b
        , m)
      run'
    JumpIfFalse p1 p2 -> do
      put
        ( if fetchVal p1 b m == 0
            then fetchVal p2 b m
            else i + 3
        , b
        , m)
      run'
    LessThan p1 p2 p3 -> do
      put
        ( i + 4
        , b
        , m //
          [ ( fetchAddr p3 b m
            , if fetchVal p1 b m < fetchVal p2 b m
                then 1
                else 0)
          ])
      run'
    Equal p1 p2 p3 -> do
      put
        ( i + 4
        , b
        , m //
          [ ( fetchAddr p3 b m
            , if fetchVal p1 b m == fetchVal p2 b m
                then 1
                else 0)
          ])
      run'
    AdjustRelativeBase p1 -> do
      put (i + 2, b + fetchVal p1 b m, m)
      run'
    Halt -> return ()

runDynamic :: Program -> (TQueue Int, TQueue Int) -> IO ()
runDynamic m qs = do
  runStateT (runReaderT run' qs) (0, 0, fromList (m <> replicate (2 ^ 8) 0))
  return ()

run :: Program -> [Int] -> IO [Int]
run m input = do
  inQ <- newTQueueIO
  outQ <- newTQueueIO
  traverse_ (atomically . writeTQueue inQ) input
  _ <- runDynamic m (inQ, outQ)
  atomically $ flushTQueue outQ
