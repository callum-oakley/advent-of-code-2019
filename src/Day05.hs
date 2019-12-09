module Day05 where

import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit        hiding (assert)
import           Test.Tasty.QuickCheck

import qualified Intcode

memory :: IO Intcode.Memory
memory = Intcode.parse <$> readFile "data/input05"

part1 :: IO Int
part1 = do
  m <- memory
  last <$> Intcode.run m [1]

part2 :: IO Int
part2 = do
  m <- memory
  last <$> Intcode.run m [5]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day05"
    [ testCase "diagnostics" $ do
        m <- memory
        out <- Intcode.run m [1]
        init out @?= [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , testProperty "run 3,0,4,0,99" $
      monadicIO $ do
        x <- pick arbitrary
        out <- run $ Intcode.run (Intcode.parse "3,0,4,0,99") [x]
        assert $ out == [x]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $
      monadicIO $ do
        x <- pick arbitrary
        out <- run $ Intcode.run (Intcode.parse "3,9,8,9,10,9,4,9,99,-1,8") [x]
        assert $
          out ==
          [ if x == 8
              then 1
              else 0
          ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $
      monadicIO $ do
        x <- pick arbitrary
        out <- run $ Intcode.run (Intcode.parse "3,9,7,9,10,9,4,9,99,-1,8") [x]
        assert $
          out ==
          [ if x < 8
              then 1
              else 0
          ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $
      monadicIO $ do
        x <- pick arbitrary
        out <- run $ Intcode.run (Intcode.parse "3,3,1108,-1,8,3,4,3,99") [x]
        assert $
          out ==
          [ if x == 8
              then 1
              else 0
          ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $
      monadicIO $ do
        x <- pick arbitrary
        out <- run $ Intcode.run (Intcode.parse "3,3,1107,-1,8,3,4,3,99") [x]
        assert $
          out ==
          [ if x < 8
              then 1
              else 0
          ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $
      monadicIO $ do
        x <- pick arbitrary
        out <-
          run $
          Intcode.run
            (Intcode.parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
            [x]
        assert $
          out ==
          [ if x /= 0
              then 1
              else 0
          ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $
      monadicIO $ do
        x <- pick arbitrary
        out <-
          run $
          Intcode.run (Intcode.parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") [x]
        assert $
          out ==
          [ if x /= 0
              then 1
              else 0
          ]
    , testProperty "sample input" $
      monadicIO $ do
        x <- pick arbitrary
        m <- run $ Intcode.parse <$> readFile "data/test05"
        out <- run $ Intcode.run m [x]
        assert $
          out ==
          [ case compare x 8 of
              LT -> 999
              EQ -> 1000
              GT -> 1001
          ]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 14155342
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 8684145
    ]
