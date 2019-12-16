module Day16 where

import           Data.Char
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           Test.Tasty
import           Test.Tasty.HUnit

coefficient :: Int -> Int -> Int
coefficient i j =
  case ((i + 1) `div` (j + 1)) `mod` 4 of
    0 -> 0
    1 -> 1
    2 -> 0
    3 -> -1

step :: Vector Int -> Vector Int
step s =
  V.generate
    (length s)
    (\j -> (`mod` 10) . abs . sum $ V.imap (\i x -> x * coefficient i j) s)

fft :: String -> String
fft =
  foldr (:) "" .
  fmap intToDigit . (!! 100) . iterate step . V.fromList . map digitToInt

part1' :: String -> String
part1' = take 8 . fft

part2' :: String -> String
part2' s = take 8 $ drop (read $ take 7 full) full
  where
    full = fft . concat $ replicate 10000 s

part1 :: IO String
part1 = part1' . takeWhile (/= '\n') <$> readFile "data/input16"

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day16"
    [ testCase "part1'" $ do
        part1' "80871224585914546619083218645595" @?= "24176176"
        part1' "19617804207202209144916044189917" @?= "73745418"
        part1' "69317163492948606335995924319873" @?= "52432133"
    -- Disabled because part2' as written is far too slow.
    -- , testCase "part2'" $ do
    --     part2' "80871224585914546619083218645595" @?= "84462026"
    --     part2' "19617804207202209144916044189917" @?= "78725270"
    --     part2' "69317163492948606335995924319873" @?= "53553731"
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= "94935919"
    ]
