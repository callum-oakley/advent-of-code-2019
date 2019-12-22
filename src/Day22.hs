module Day22 where

import           Control.Monad
import           Data.Either
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec          (Parsec, (<|>))
import qualified Text.Parsec          as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.Token    as Parsec

data Technique
  = DealIntoNewStack
  | Cut Integer
  | DealWithIncrement Integer
  deriving (Show)

data Shuffle =
  Shuffle Integer Integer -- Shuffle a b represents a linear function \x -> a x + b
  deriving (Show)

parseTechnique :: Parsec String () Technique
parseTechnique =
  Parsec.string "cut " *> (Cut <$> integer) <|>
  Parsec.string "deal " *>
  (Parsec.string "into new stack" $> DealIntoNewStack <|>
   Parsec.string "with increment " *> (DealWithIncrement <$> integer))
  where
    integer = Parsec.integer Parsec.haskell

fromTechnique :: Technique -> Shuffle
fromTechnique DealIntoNewStack      = Shuffle (-1) (-1)
fromTechnique (Cut n)               = Shuffle 1 (-n)
fromTechnique (DealWithIncrement n) = Shuffle n 0

compose :: Integer -> Shuffle -> Shuffle -> Shuffle
compose deckSize (Shuffle a b) (Shuffle c d) =
  Shuffle ((c * a) `mod` deckSize) ((c * b + d) `mod` deckSize)

parse :: Integer -> String -> Shuffle
parse deckSize =
  foldr1 (compose deckSize) .
  map fromTechnique . rights . map (Parsec.parse parseTechnique "") . lines

apply :: Integer -> Shuffle -> Integer -> Integer
apply deckSize (Shuffle a b) card = (a * card + b) `mod` deckSize

-- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
modInv :: Integer -> Integer -> Integer
modInv n m = go 0 1 m n
  where
    go t _ _ 0 =
      if t < 0
        then t + m
        else t
    go t t' r r' = go t' (t - q * t') r' (r - q * r')
      where
        q = r `div` r'

inverse :: Integer -> Shuffle -> Shuffle
inverse deckSize (Shuffle a b) = Shuffle a' (-b * a' `mod` deckSize)
  where
    a' = modInv a deckSize

pow :: Integer -> Shuffle -> Integer -> Shuffle
pow _ _ 0 = Shuffle 1 0
pow _ shuffle 1 = shuffle
pow deckSize shuffle k
  | k < 0 = pow deckSize (inverse deckSize shuffle) (-k)
  | otherwise =
    if even k
      then compose deckSize shuffle' shuffle'
      else compose deckSize shuffle' (compose deckSize shuffle shuffle')
  where
    shuffle' = pow deckSize shuffle (k `div` 2)

part1 :: IO Integer
part1 = do
  shuffle <- parse deckSize <$> readFile "data/input22"
  pure $ apply deckSize shuffle 2019
  where
    deckSize = 10007

part2 :: IO Integer
part2 = do
  firstPass <- parse deckSize <$> readFile "data/input22"
  let shuffle = pow deckSize firstPass (-101741582076661)
  pure $ apply deckSize shuffle 2020
  where
    deckSize = 119315717514047

-- just for testing
simulateFullShuffle :: Integer -> Shuffle -> [Int]
simulateFullShuffle deckSize shuffle =
  map (fromJust . flip elemIndex positions) [0 .. deckSize - 1]
  where
    positions = map (apply deckSize shuffle) [0 .. deckSize - 1]

testData :: [String]
testData =
  map
    unlines
    [ ["deal into new stack"]
    , ["cut 3"]
    , ["cut -4"]
    , ["deal with increment 3"]
    , ["deal with increment 7", "deal into new stack", "deal into new stack"]
    , ["cut 6", "deal with increment 7", "deal into new stack"]
    , ["deal with increment 7", "deal with increment 9", "cut -2"]
    , [ "deal into new stack"
      , "cut -2"
      , "deal with increment 7"
      , "cut 8"
      , "cut -4"
      , "deal with increment 7"
      , "cut 3"
      , "deal with increment 9"
      , "deal with increment 3"
      , "cut -1"
      ]
    ]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day22"
    [ testCase "shuffle" $
      zipWithM_
        (\shuffle expected -> simulateFullShuffle 10 shuffle @?= expected)
        (map (parse 10) testData)
        [ [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        , [3, 4, 5, 6, 7, 8, 9, 0, 1, 2]
        , [6, 7, 8, 9, 0, 1, 2, 3, 4, 5]
        , [0, 7, 4, 1, 8, 5, 2, 9, 6, 3]
        , [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
        , [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]
        , [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]
        , [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]
        ]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 8775
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 47141544607176
    ]
