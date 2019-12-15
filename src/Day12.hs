module Day12 where

import           Data.Maybe
import           Linear.V3
import           Test.Tasty
import           Test.Tasty.HUnit

type Position = V3 Int

type Velocity = V3 Int

data Moon =
  Moon Position Velocity
  deriving (Eq, Show)

data Axis
  = X
  | Y
  | Z

gravity :: [Moon] -> [Moon]
gravity ms = map gravity' ms
  where
    gravity' (Moon p v) = Moon p (v + sum [delta p p' | (Moon p' _) <- ms])
    delta (V3 x y z) (V3 x' y' z') = V3 (pull x x') (pull y y') (pull z z')
    pull a b =
      case compare a b of
        LT -> 1
        EQ -> 0
        GT -> -1

velocity :: [Moon] -> [Moon]
velocity = map (\(Moon p v) -> Moon (p + v) v)

step :: [Moon] -> [Moon]
step = velocity . gravity

potentialEnergy :: Moon -> Int
potentialEnergy (Moon p _) = sum $ fmap abs p

kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ v) = sum $ fmap abs v

totalEnergy :: Moon -> Int
totalEnergy m = potentialEnergy m * kineticEnergy m

part1' :: [Moon] -> Int -> Int
part1' ms n = sum . map totalEnergy $ iterate step ms !! n

project :: Axis -> Moon -> (Int, Int)
project i (Moon (V3 px py pz) (V3 vx vy vz)) =
  case i of
    X -> (px, vx)
    Y -> (py, vy)
    Z -> (pz, vz)

part2' :: [Moon] -> Int
part2' ms = go Nothing Nothing Nothing 1 (step ms)
  where
    go (Just i) (Just j) (Just k) _ _ = foldl1 lcm [i, j, k]
    go x y z i ms' =
      go (f x X i ms') (f y Y i ms') (f z Z i ms') (i + 1) (step ms')
    f a axis i ms'
      | isJust a = a
      | map (project axis) ms' == map (project axis) ms = Just i
      | otherwise = Nothing

moons :: [Moon]
moons =
  [ Moon (V3 5 (-1) 5) 0
  , Moon (V3 0 (-14) 2) 0
  , Moon (V3 16 4 0) 0
  , Moon (V3 18 1 16) 0
  ]

part1 :: Int
part1 = part1' moons 1000

part2 :: Int
part2 = part2' moons

testMoons1 :: [Moon]
testMoons1 =
  [ Moon (V3 (-1) 0 2) 0
  , Moon (V3 2 (-10) (-7)) 0
  , Moon (V3 4 (-8) 8) 0
  , Moon (V3 3 5 (-1)) 0
  ]

testMoons2 :: [Moon]
testMoons2 =
  [ Moon (V3 (-8) (-10) 0) 0
  , Moon (V3 5 5 10) 0
  , Moon (V3 2 (-7) 3) 0
  , Moon (V3 9 (-8) (-3)) 0
  ]

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day12"
    [ testCase "part1'" $ do
        part1' testMoons1 10 @?= 179
        part1' testMoons2 100 @?= 1940
    , testCase "part1" $ part1 @?= 7928
    , testCase "part2'" $ do
        part2' testMoons1 @?= 2772
        part2' testMoons2 @?= 4686774924
    , testCase "part2" $ part2 @?= 518311327635164
    ]
