{-# LANGUAGE TupleSections #-}

module Day07 where

import           Control.Monad.RWS
import           Data.List
import           Data.List.Split
import           Data.Vector           (Vector, fromList, (!), (//))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

type Memory = Vector Int

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

run' :: RWS (Vector Int) [Int] (Int, Int, Memory) ()
run' = do
  (i, j, m) <- get
  case instruction j $ m ! j of
    Add p1 p2 p3 -> do
      put (i, j + 4, m // [(fetchAddr p3 m, fetchVal p1 m + fetchVal p2 m)])
      run'
    Multiply p1 p2 p3 -> do
      put (i, j + 4, m // [(fetchAddr p3 m, fetchVal p1 m * fetchVal p2 m)])
      run'
    Receive p1 -> do
      input <- ask
      put (i + 1, j + 2, m // [(fetchAddr p1 m, input ! i)])
      run'
    Send p1 -> do
      tell [fetchVal p1 m]
      put (i, j + 2, m)
      run'
    JumpIfTrue p1 p2 -> do
      put
        ( i
        , if fetchVal p1 m /= 0
            then fetchVal p2 m
            else j + 3
        , m)
      run'
    JumpIfFalse p1 p2 -> do
      put
        ( i
        , if fetchVal p1 m == 0
            then fetchVal p2 m
            else j + 3
        , m)
      run'
    LessThan p1 p2 p3 -> do
      put
        ( i
        , j + 4
        , m //
          [ ( fetchAddr p3 m
            , if fetchVal p1 m < fetchVal p2 m
                then 1
                else 0)
          ])
      run'
    Equal p1 p2 p3 -> do
      put
        ( i
        , j + 4
        , m //
          [ ( fetchAddr p3 m
            , if fetchVal p1 m == fetchVal p2 m
                then 1
                else 0)
          ])
      run'
    Halt -> return ()

run :: Vector Int -> Memory -> [Int]
run input = snd . execRWS run' input . (0, 0, )

type Signal = Int

type Phase = Int

amp :: Phase -> Signal -> Memory -> Signal
amp phase signal = head . run (fromList [phase, signal])

ampChain :: Memory -> [Phase] -> Signal
ampChain m phases = foldl (\signal phase -> amp phase signal m) 0 phases

part1' :: Memory -> Int
part1' m = maximum . map (ampChain m) . permutations $ [0 .. 4]

memory :: IO Memory
memory = parse <$> readFile "data/input07"

part1 :: IO Int
part1 = part1' <$> memory

test =
  defaultMain $
  testGroup
    "day07"
    [ testCase "part1'" $ do
        part1' (parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") @?=
          43210
        part1'
          (parse $
           "3,23,3,24,1002,24,10,24,1002,23,-1,23," <>
           "101,5,23,23,1,24,23,23,4,23,99,0,0") @?=
          54321
        part1'
          (parse $
           "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," <>
           "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") @?=
          65210
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 255590
    ]
