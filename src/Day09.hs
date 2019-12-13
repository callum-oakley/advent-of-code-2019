module Day09 where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input09"

part1 :: IO Int
part1 = do
  p <- program
  return . head $ Intcode.run p [1]

part2 :: IO Int
part2 = do
  p <- program
  return . head $ Intcode.run p [2]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day09"
    [ testCase "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" $ do
        let p =
              Intcode.parse
                "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        Intcode.run p [] @?= p
    , testCase "1102,34915192,34915192,7,4,7,99,0" $ do
        let p = Intcode.parse "1102,34915192,34915192,7,4,7,99,0"
        let out = Intcode.run p []
        length out @?= 1
        length (show $ head out) @?= 16
    , testCase "104,1125899906842624,99" $
      Intcode.run (Intcode.parse "104,1125899906842624,99") [] @?=
      [1125899906842624]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3989758265
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 76791
    ]
