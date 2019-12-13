module Day07 where

import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

type Signal = Int

type Phase = Int

ampLoop :: Intcode.Program -> [Phase] -> Signal
ampLoop p [a, b, c, d, e] = last outE
  where
    outA = Intcode.run p (a : 0 : outE)
    outB = Intcode.run p (b : outA)
    outC = Intcode.run p (c : outB)
    outD = Intcode.run p (d : outC)
    outE = Intcode.run p (e : outD)

part1' :: Intcode.Program -> Signal
part1' p = maximum . map (ampLoop p) . permutations $ [0 .. 4]

part2' :: Intcode.Program -> Signal
part2' p = maximum . map (ampLoop p) . permutations $ [5 .. 9]

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input07"

part1 :: IO Signal
part1 = part1' <$> program

part2 :: IO Signal
part2 = part2' <$> program

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day07"
    [ testCase "part1'" $ do
        part1' (Intcode.parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0") @?=
          43210
        part1'
          (Intcode.parse $
           "3,23,3,24,1002,24,10,24,1002,23,-1,23," <>
           "101,5,23,23,1,24,23,23,4,23,99,0,0") @?=
          54321
        part1'
          (Intcode.parse $
           "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," <>
           "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0") @?=
          65210
    , testCase "part2'" $ do
        part2'
          (Intcode.parse $
           "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26," <>
           "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5") @?=
          139629729
        part2'
          (Intcode.parse $
           "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54," <>
           "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4," <>
           "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10") @?=
          18216
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 255590
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 58285150
    ]
