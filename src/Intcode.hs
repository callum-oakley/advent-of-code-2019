module Intcode
  ( parse
  , run
  , runStatic
  , Memory
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import           Data.List.Split
import           Data.Vector            (Vector, fromList, (!), (//))

type Memory = Vector Int

type Machine = (Int, Memory)

data Mode
  = Position
  | Immediate
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
  | Halt
  deriving (Show, Eq)

parse :: String -> Memory
parse = fromList . map read . splitOn ","

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
    99 -> Halt
  where
    param n m =
      case n `div` (10 ^ (m + 1)) `mod` 10 of
        0 -> Parameter Position (i + m)
        1 -> Parameter Immediate (i + m)

fetchVal :: Parameter -> Memory -> Int
fetchVal p m = m ! fetchAddr p m

fetchAddr :: Parameter -> Memory -> Int
fetchAddr (Parameter Position i) m  = m ! i
fetchAddr (Parameter Immediate i) _ = i

run' :: ReaderT (TQueue Int, TQueue Int) (StateT Machine IO) ()
run' = do
  (i, m) <- get
  case instruction i $ m ! i of
    Add p1 p2 p3 -> do
      put (i + 4, m // [(fetchAddr p3 m, fetchVal p1 m + fetchVal p2 m)])
      run'
    Multiply p1 p2 p3 -> do
      put (i + 4, m // [(fetchAddr p3 m, fetchVal p1 m * fetchVal p2 m)])
      run'
    Receive p1 -> do
      inQ <- asks fst
      input <- lift . lift . atomically . readTQueue $ inQ
      put (i + 2, m // [(fetchAddr p1 m, input)])
      run'
    Send p1 -> do
      outQ <- asks snd
      lift . lift . atomically . writeTQueue outQ $ fetchVal p1 m
      put (i + 2, m)
      run'
    JumpIfTrue p1 p2 -> do
      put
        ( if fetchVal p1 m /= 0
            then fetchVal p2 m
            else i + 3
        , m)
      run'
    JumpIfFalse p1 p2 -> do
      put
        ( if fetchVal p1 m == 0
            then fetchVal p2 m
            else i + 3
        , m)
      run'
    LessThan p1 p2 p3 -> do
      put
        ( i + 4
        , m //
          [ ( fetchAddr p3 m
            , if fetchVal p1 m < fetchVal p2 m
                then 1
                else 0)
          ])
      run'
    Equal p1 p2 p3 -> do
      put
        ( i + 4
        , m //
          [ ( fetchAddr p3 m
            , if fetchVal p1 m == fetchVal p2 m
                then 1
                else 0)
          ])
      run'
    Halt -> return ()

run :: Memory -> (TQueue Int, TQueue Int) -> IO ((), Machine)
run m qs = runStateT (runReaderT run' qs) (0, m)

runStatic :: Memory -> [Int] -> IO [Int]
runStatic m input = do
  inQ <- newTQueueIO
  outQ <- newTQueueIO
  traverse_ (atomically . writeTQueue inQ) input
  _ <- run m (inQ, outQ)
  atomically $ flushTQueue outQ
