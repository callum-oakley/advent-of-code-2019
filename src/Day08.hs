module Day08 where

import           Data.Char
import           Data.Foldable
import           Data.List.Split
import           Data.Ord
import           Test.Tasty
import           Test.Tasty.HUnit

type Layer = [Int]

type Image = [Layer]

parse :: (Int, Int) -> String -> Image
parse (x, y) = chunksOf (x * y) . map digitToInt . takeWhile isDigit

image :: IO Image
image = parse (25, 6) <$> readFile "data/input08"

flatten :: Image -> Layer
flatten = foldl1 (zipWith flattenPixel)
  where
    flattenPixel 2 p = p
    flattenPixel p _ = p

part1 :: IO Int
part1 = do
  i <- image
  let count target = length . filter (== target)
  let layer = minimumBy (comparing (count 0)) i
  return $ count 1 layer * count 2 layer

part2' :: IO String
part2' = unlines . map printRow . chunksOf 25 . flatten <$> image
  where
    printRow = concatMap draw
    draw 0 = "   "
    draw 1 = "███"

part2 :: IO ()
part2 = putStrLn =<< part2'

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day08"
    [ testCase "parse" $
      parse (2, 3) "123456789012" @?= [[1, 2, 3, 4, 5, 6], [7, 8, 9, 0, 1, 2]]
    , testCase "flatten" $
      flatten (parse (2, 2) "0222112222120000") @?= [0, 1, 1, 0]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 1452
    , testCase "part2'" $ do
        p2 <- part2'
        p2 @?=
          unlines
            [ "█████████      ███      ███   █████████      ████████████   ███      ███   "
            , "███      ███   ███      ███   ███      ███   ███            ███      ███   "
            , "███      ███   ████████████   ███      ███   █████████      ███      ███   "
            , "█████████      ███      ███   █████████      ███            ███      ███   "
            , "███            ███      ███   ███            ███            ███      ███   "
            , "███            ███      ███   ███            ████████████      ██████      "
            ]
    ]
