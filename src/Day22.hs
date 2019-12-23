{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Day22 where

import           Control.Monad
import           Data.Either
import           Data.Functor
import           Data.Group
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Ratio
import           GHC.TypeLits         hiding (Mod)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec          (Parsec, (<|>))
import qualified Text.Parsec          as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.Token    as Parsec

newtype Mod (m :: Nat) =
  Mod Integer
  deriving (Show, Eq)

unMod :: Mod (m :: Nat) -> Integer
unMod (Mod n) = n

instance forall m. KnownNat m => Num (Mod m) where
  fromInteger x = Mod $ x `mod` natVal (Proxy :: Proxy m)
  (Mod x) + (Mod y) = fromInteger $ x + y
  (Mod x) * (Mod y) = fromInteger $ x * y
  (Mod x) - (Mod y) = fromInteger $ x - y
  abs = id
  signum = const 1

instance KnownNat m => Fractional (Mod m) where
  recip x = x ^ (natVal x - 2) -- True for prime m by Fermat's little theorem
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

data Shuffle (m :: Nat) =
  Shuffle (Mod m) (Mod m) -- Shuffle a b represents a linear function \x -> a x + b
  deriving (Show)

base :: KnownNat m => Shuffle m -> Integer
base (Shuffle a _) = natVal a

instance KnownNat m => Semigroup (Shuffle m) where
  (Shuffle a b) <> (Shuffle c d) = Shuffle (c * a) (c * b + d)

instance KnownNat m => Monoid (Shuffle m) where
  mempty = Shuffle 1 0

instance KnownNat m => Group (Shuffle m) where
  invert (Shuffle a b) = Shuffle (1 / a) (-b / a)

data Technique
  = DealIntoNewStack
  | Cut Integer
  | DealWithIncrement Integer
  deriving (Show)

parseTechnique :: Parsec String () Technique
parseTechnique =
  Parsec.string "cut " *> (Cut <$> integer) <|>
  Parsec.string "deal " *>
  (Parsec.string "into new stack" $> DealIntoNewStack <|>
   Parsec.string "with increment " *> (DealWithIncrement <$> integer))
  where
    integer = Parsec.integer Parsec.haskell

fromTechnique :: KnownNat m => Technique -> Shuffle m
fromTechnique DealIntoNewStack      = Shuffle (-1) (-1)
fromTechnique (Cut n)               = Shuffle 1 (-fromInteger n)
fromTechnique (DealWithIncrement n) = Shuffle (fromInteger n) 0

parse :: KnownNat m => String -> Shuffle m
parse =
  mconcat .
  map fromTechnique . rights . map (Parsec.parse parseTechnique "") . lines

apply :: KnownNat m => Shuffle m -> Mod m -> Mod m
apply (Shuffle a b) card = a * card + b

part1 :: IO (Mod 10007)
part1 = do
  shuffle <- parse <$> readFile "data/input22"
  pure $ apply shuffle 2019

part2 :: IO (Mod 119315717514047)
part2 = do
  shuffle <- flip pow (-101741582076661) . parse <$> readFile "data/input22"
  pure $ apply shuffle 2020

-- just for testing
simulateFullShuffle :: KnownNat m => Shuffle m -> [Int]
simulateFullShuffle shuffle =
  map
    (fromJust . flip elemIndex positions . fromInteger)
    [0 .. base shuffle - 1]
  where
    positions = map (apply shuffle . fromInteger) [0 .. base shuffle - 1]

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
        (\shuffle expected -> simulateFullShuffle @10 shuffle @?= expected)
        (map parse testData)
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
