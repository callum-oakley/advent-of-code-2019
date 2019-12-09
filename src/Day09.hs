module Day09 where

import           Data.Vector      hiding (head, length)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

memory :: IO Intcode.Memory
memory = Intcode.parse <$> readFile "data/input09"

part1 :: IO Int
part1 = do
  m <- memory
  head <$> Intcode.run m [1]

part2 :: IO Int
part2 = do
  m <- memory
  head <$> Intcode.run m [2]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day09"
    [ testCase "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" $ do
        let m =
              Intcode.parse
                "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        x <- Intcode.run m []
        x @?= toList m
    , testCase "1102,34915192,34915192,7,4,7,99,0" $ do
        let m = Intcode.parse "1102,34915192,34915192,7,4,7,99,0"
        x <- Intcode.run m []
        length x @?= 1
        length (show $ head x) @?= 16
    , testCase "104,1125899906842624,99" $ do
        let m = Intcode.parse "104,1125899906842624,99"
        x <- Intcode.run m []
        x @?= [1125899906842624]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3989758265
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 76791
    ]
