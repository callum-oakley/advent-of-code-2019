module Day02 where

import           Data.List.Split
import           Data.Vector      (Vector, fromList, (!), (//))
import           Test.Tasty
import           Test.Tasty.HUnit

type Program = Vector Int

parse :: String -> Program
parse = fromList . map read . splitOn ","

(!~) :: Program -> Int -> Int
p !~ i = p ! (p ! i)

step :: (Int, Program) -> Maybe (Int, Program)
step (i, p) =
  case p ! i of
    1  -> Just (i + 4, p // [(p ! (i + 3), (p !~ (i + 1)) + (p !~ (i + 2)))])
    2  -> Just (i + 4, p // [(p ! (i + 3), (p !~ (i + 1)) * (p !~ (i + 2)))])
    99 -> Nothing

run :: Program -> Program
run p = go (0, p)
  where
    go (i, q) =
      case step (i, q) of
        Just (i', q') -> go (i', q')
        Nothing       -> q

runWith :: Int -> Int -> Program -> Program
runWith noun verb p = run (p // [(1, noun), (2, verb)])

program :: IO Program
program = parse <$> readFile "data/input02"

part1 :: IO Int
part1 = (! 0) . runWith 12 2 <$> program

part2 :: IO Int
part2 = do
  p <- program
  let (noun, verb) =
        head
          [ (n, v)
          | n <- [0 .. 99]
          , v <- [0 .. 99]
          , runWith n v p ! 0 == 19690720
          ]
  return $ 100 * noun + verb

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day02"
    [ testCase "step" $ do
        step (0, parse "1,9,10,3,2,3,11,0,99,30,40,50") @?=
          Just (4, parse "1,9,10,70,2,3,11,0,99,30,40,50")
        step (4, parse "1,9,10,70,2,3,11,0,99,30,40,50") @?=
          Just (8, parse "3500,9,10,70,2,3,11,0,99,30,40,50")
        step (8, parse "3500,9,10,70,2,3,11,0,99,30,40,50") @?= Nothing
    , testCase "run" $ do
        run (parse "1,0,0,0,99") @?= parse "2,0,0,0,99"
        run (parse "2,3,0,3,99") @?= parse "2,3,0,6,99"
        run (parse "2,4,4,5,99,0") @?= parse "2,4,4,5,99,9801"
        run (parse "1,1,1,4,99,5,6,0,99") @?= parse "30,1,1,4,2,5,6,0,99"
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 3562672
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 8250
    ]
