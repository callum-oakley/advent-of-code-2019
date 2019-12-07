module Day07 where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Vector                   (Vector, fromList, (!), (//))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

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

type Signal = Int

type Phase = Int

ampChain :: Memory -> [Phase] -> IO Signal
ampChain m = foldM step 0
  where
    step signal phase = do
      inQ <- newTQueueIO
      outQ <- newTQueueIO
      traverse_ (atomically . writeTQueue inQ) [phase, signal]
      run m (inQ, outQ)
      atomically $ readTQueue outQ

ampLoop :: Memory -> [Phase] -> IO Signal
ampLoop m phases = do
  queues <- sequenceA . replicate 5 $ newTQueueIO
  sequenceA_ $
    zipWith (\q phase -> atomically $ writeTQueue q phase) queues phases
  atomically $ writeTQueue (head queues) 0
  amps <- mapM (async . run m) $ zip queues (tail $ cycle queues)
  mapM_ wait amps
  atomically $ readTQueue (head queues)

part1' :: Memory -> IO Signal
part1' m = fmap maximum . mapM (ampChain m) . permutations $ [0 .. 4]

part2' :: Memory -> IO Signal
part2' m = fmap maximum . mapM (ampLoop m) . permutations $ [5 .. 9]

memory :: IO Memory
memory = parse <$> readFile "data/input07"

part1 :: IO Signal
part1 = part1' =<< memory

part2 :: IO Signal
part2 = part2' =<< memory

test =
  defaultMain $
  testGroup
    "day07"
    [ testCase "part1'" $ do
        x <- part1' (parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
        x @?= 43210
        y <-
          part1'
            (parse $
             "3,23,3,24,1002,24,10,24,1002,23,-1,23," <>
             "101,5,23,23,1,24,23,23,4,23,99,0,0")
        y @?= 54321
        z <-
          part1'
            (parse $
             "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," <>
             "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
        z @?= 65210
    , testCase "part2'" $ do
        x <-
          part2'
            (parse $
             "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26," <>
             "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
        x @?= 139629729
        y <-
          part2'
            (parse $
             "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54," <>
             "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4," <>
             "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
        y @?= 18216
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 255590
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 58285150
    ]
