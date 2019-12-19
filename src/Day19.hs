module Day19 where

import           Linear.V2
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

inBeam :: Intcode.Program -> V2 Int -> Bool
inBeam p (V2 x y) =
  case head $ Intcode.run p [x, y] of
    0 -> False
    1 -> True

part1 :: IO Int
part1 = do
  p <- Intcode.parse <$> readFile "data/input19"
  pure $ length $ filter (inBeam p) [V2 x y | x <- [0 .. 49], y <- [0 .. 49]]

part2 :: IO Int
part2 = do
  p <- Intcode.parse <$> readFile "data/input19"
  let V2 x y = go p (head $ filter (inBeam p) [V2 x' 99 | x' <- [0 ..]])
  pure $ 10000 * x + y
  where
    go p pos
      | inBeam p (pos + V2 99 (-99)) = pos + V2 0 (-99)
      | otherwise = go p (head $ filter (inBeam p) [pos + V2 x 1 | x <- [0 ..]])

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day19"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 116
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 10311666
    ]
