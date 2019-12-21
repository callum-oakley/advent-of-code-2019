module Intcode
  ( parse
  , run
  , runDynamic
  , runInteractive
  , runInteractiveAscii
  , expectInput
  , expectOutput
  , Program
  , Effect(..)
  ) where

import           Data.Char
import           Data.List.Split
import           Data.Vector     (Vector, fromList, (!), (//))

type Program = [Int]

type Memory = Vector Int

type RelativeBase = Int

type Machine = (Int, RelativeBase, Memory)

data Effect
  = Input (Int -> Effect)
  | Output Int Effect
  | Stop

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
    param m k =
      case m `div` (10 ^ (k + 1)) `mod` 10 of
        0 -> Parameter Position (i + k)
        1 -> Parameter Immediate (i + k)
        2 -> Parameter Relative (i + k)

fetchVal :: Parameter -> RelativeBase -> Memory -> Int
fetchVal p b m = m ! fetchAddr p b m

fetchAddr :: Parameter -> RelativeBase -> Memory -> Int
fetchAddr (Parameter Position i) _ m  = m ! i
fetchAddr (Parameter Immediate i) _ _ = i
fetchAddr (Parameter Relative i) b m  = b + m ! i

run' :: Machine -> Effect
run' (i, b, m) =
  case instruction i $ m ! i of
    Add p1 p2 p3 ->
      run'
        (i + 4, b, m // [(fetchAddr p3 b m, fetchVal p1 b m + fetchVal p2 b m)])
    Multiply p1 p2 p3 ->
      run'
        (i + 4, b, m // [(fetchAddr p3 b m, fetchVal p1 b m * fetchVal p2 b m)])
    Receive p1 ->
      Input (\input -> run' (i + 2, b, m // [(fetchAddr p1 b m, input)]))
    Send p1 -> Output (fetchVal p1 b m) (run' (i + 2, b, m))
    JumpIfTrue p1 p2 ->
      run'
        ( if fetchVal p1 b m /= 0
            then fetchVal p2 b m
            else i + 3
        , b
        , m)
    JumpIfFalse p1 p2 ->
      run'
        ( if fetchVal p1 b m == 0
            then fetchVal p2 b m
            else i + 3
        , b
        , m)
    LessThan p1 p2 p3 ->
      run'
        ( i + 4
        , b
        , m //
          [ ( fetchAddr p3 b m
            , if fetchVal p1 b m < fetchVal p2 b m
                then 1
                else 0)
          ])
    Equal p1 p2 p3 ->
      run'
        ( i + 4
        , b
        , m //
          [ ( fetchAddr p3 b m
            , if fetchVal p1 b m == fetchVal p2 b m
                then 1
                else 0)
          ])
    AdjustRelativeBase p1 -> run' (i + 2, b + fetchVal p1 b m, m)
    Halt -> Stop

runDynamic :: Program -> Effect
runDynamic p = run' (0, 0, fromList (p <> replicate (2 ^ 11) 0))

run :: Program -> [Int] -> [Int]
run p = go (runDynamic p)
  where
    go (Input f) (i:input) = go (f i) input
    go (Output o e) input  = o : go e input
    go Stop _              = []

runInteractive :: Program -> IO ()
runInteractive = go . runDynamic
  where
    go (Input f)     = getLine >>= go . f . read
    go (Output o c') = print o >> go c'
    go Stop          = pure ()

runInteractiveAscii :: Program -> IO ()
runInteractiveAscii = go . runDynamic
  where
    go (Input f)     = getChar >>= go . f . ord
    go (Output o c') = putStr (display o) >> go c'
    go Stop          = pure ()
    display o
      | o <= 128 = [chr o]
      | otherwise = show o

-- Partial function for ergonomics when we know a priori that our machine is
-- expecting an input.
expectInput :: Effect -> Int -> Effect
expectInput (Input f) = f

-- Partial function for ergonomics when we know a priori that our machine is
-- expecting an output.
expectOutput :: Effect -> (Int, Effect)
expectOutput (Output o e) = (o, e)
