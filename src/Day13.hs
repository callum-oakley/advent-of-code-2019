module Day13 where

import           Data.List.Split
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

play :: Intcode.Program -> Int
play p = last . map (!! 2) . filter (\[x, y, _] -> x == -1 && y == 0) $ output
  where
    output = chunksOf 3 $ Intcode.run p input
    input = mapMaybe snd $ scanl step (Nothing, Nothing) output
    step (paddleX, _) [x, _, tile] =
      case tile of
        3 -> (Just x, Nothing)
        4 ->
          ( paddleX
          , case paddleX of
              Just paddleX' -> Just . signum $ x - paddleX'
              Nothing       -> Just 0)
        _ -> (paddleX, Nothing)

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
