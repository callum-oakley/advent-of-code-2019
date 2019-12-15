module Day13 where

import           Data.List.Split
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

readChunksOf :: Int -> Intcode.Effect -> ([Int], Intcode.Effect)
readChunksOf 0 c = ([], c)
readChunksOf n c = (o : os, c'')
  where
    (o, c') = Intcode.expectOutput c
    (os, c'') = readChunksOf (n - 1) c'

play :: Intcode.Program -> Int
play p = go (Intcode.runDynamic p) 0 0 0
  where
    go c paddleX ballX score =
      case c of
        Intcode.Input f -> go (f . signum $ ballX - paddleX) paddleX ballX score
        Intcode.Output _ _ ->
          case readChunksOf 3 c of
            ([-1, 0, z], c') -> go c' paddleX ballX z
            ([x, _, 3], c')  -> go c' x ballX score
            ([x, _, 4], c')  -> go c' paddleX x score
            (_, c')          -> go c' paddleX ballX score
        Intcode.Stop -> score

program :: IO Intcode.Program
program = Intcode.parse <$> readFile "data/input13"

part1 :: IO Int
part1 = do
  p <- program
  return . length . filter (\[_, _, n] -> n == 2) . chunksOf 3 $
    Intcode.run p []

part2 :: IO Int
part2 = do
  p <- program
  return . play $ 2 : tail p

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day13"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 452
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 21415
    ]
