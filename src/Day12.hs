module Day12 where

import           Linear.V3
import           Test.Tasty
import           Test.Tasty.HUnit

type Position = V3 Int

type Velocity = V3 Int

data Moon =
  Moon Position Velocity
  deriving (Eq, Show)

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

projectX (Moon (V3 px _ _) (V3 vx _ _)) = (px, vx)

projectY (Moon (V3 _ py _) (V3 _ vy _)) = (py, vy)

projectZ (Moon (V3 _ _ pz) (V3 _ _ vz)) = (pz, vz)

part2' ms =
  foldl1
    lcm
    [ fst .
      head .
      filter (\(_, ms') -> map projectX ms' == map projectX ms) .
      tail . zip [0 ..] $
      iterate step ms
    , fst .
      head .
      filter (\(_, ms') -> map projectY ms' == map projectY ms) .
      tail . zip [0 ..] $
      iterate step ms
    , fst .
      head .
      filter (\(_, ms') -> map projectZ ms' == map projectZ ms) .
      tail . zip [0 ..] $
      iterate step ms
    ]

moons :: [Moon]
moons =
  [ Moon (V3 5 (-1) 5) 0
  , Moon (V3 0 (-14) 2) 0
  , Moon (V3 16 4 0) 0
  , Moon (V3 18 1 16) 0
  ]

part1 :: Int
part1 = part1' moons 1000

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
    ]
