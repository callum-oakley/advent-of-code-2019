module Day05 where

import           Data.List.Split
import           Data.Vector      (Vector, fromList, (!), (//))
import           Test.Tasty
import           Test.Tasty.HUnit

type Memory = Vector Int

type Input = [Int]

type Output = [Int]

data Program =
  Program Int Memory [Int] [Int]
  deriving (Show, Eq)

data Mode
  = Position
  | Immediate
  deriving (Show, Eq)

data Instruction
  = Add Mode Mode Mode
  | Multiply Mode Mode Mode
  | Receive Mode
  | Send Mode
  | Halt
  deriving (Show, Eq)

parse :: String -> Program
parse s = Program 0 (fromList . map read . splitOn "," $ s) [] []

instruction :: Int -> Instruction
instruction n =
  case n `mod` 100 of
    1  -> Add (mode n 1) (mode n 2) (mode n 3)
    2  -> Multiply (mode n 1) (mode n 2) (mode n 3)
    3  -> Receive (mode n 1)
    4  -> Send (mode n 1)
    99 -> Halt
  where
    mode n m =
      case n `div` (10 ^ (m + 1)) `mod` 10 of
        0 -> Position
        1 -> Immediate

fetchVal :: Mode -> Memory -> Int -> Int -> Int
fetchVal mode m i offset = m ! (fetchAddr mode m i offset)

fetchAddr :: Mode -> Memory -> Int -> Int -> Int
fetchAddr Position m i offset  = m ! (i + offset)
fetchAddr Immediate m i offset = i + offset

step :: Program -> Maybe Program
step (Program i m inp out) =
  case instruction $ m ! i of
    Add mode1 mode2 mode3 ->
      Just $
      Program
        (i + 4)
        (m //
         [ ( fetchAddr mode3 m i 3
           , (fetchVal mode1 m i 1) + (fetchVal mode2 m i 2))
         ])
        inp
        out
    Multiply mode1 mode2 mode3 ->
      Just $
      Program
        (i + 4)
        (m //
         [ ( fetchAddr mode3 m i 3
           , (fetchVal mode1 m i 1) * (fetchVal mode2 m i 2))
         ])
        inp
        out
    Receive mode1 ->
      Just $
      Program (i + 2) (m // [(fetchAddr mode1 m i 1, head inp)]) (tail inp) out
    Send mode1 -> Just $ Program (i + 2) m inp ((fetchVal mode1 m i 1) : out)
    Halt -> Nothing

run' :: Program -> Program
run' p =
  case step p of
    Just p' -> run' p'
    Nothing -> p

input :: Input -> Program -> Program
input inp (Program i m _ out) = Program i m inp out

output :: Program -> Output
output (Program _ _ _ out) = out

run :: Input -> Program -> Output
run inp = output . run' . input inp

program :: IO Program
program = parse <$> readFile "data/input05"

part1 :: IO Int
part1 = head <$> run [1] <$> program

test =
  defaultMain $
  testGroup
    "day05"
    [ testCase "run'" $ do
        run' (parse "1002,4,3,4,33") @?=
          Program 4 (fromList [1002, 4, 3, 4, 99]) [] []
        run' (parse "1101,100,-1,4,0") @?=
          Program 4 (fromList [1101, 100, -1, 4, 99]) [] []
    , testCase "run" $ do
        run [5] (parse "3,0,4,0,99") @?= [5]
        run [42] (parse "3,0,4,0,99") @?= [42]
    , testCase "diagnostics" $ do
        out <- run [1] <$> program
        tail out @?= [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 14155342
    ]
