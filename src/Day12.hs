module Day12 where

import           Data.Ord
import           Linear.V3
import           Test.Tasty
import           Test.Tasty.HUnit

type Position = V3 Int

type Velocity = V3 Int

data Moon =
  Moon Position Velocity
  deriving (Eq, Show)

gravity :: [Moon] -> [Moon]
gravity moons = map gravity' moons
  where
    gravity' (Moon p v) = Moon p (v + sum [delta p p' | (Moon p' _) <- moons])
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

testMoons :: [Moon]
testMoons =
  [ Moon (V3 (-1) 0 2) 0
  , Moon (V3 2 (-10) (-7)) 0
  , Moon (V3 4 (-8) 8) 0
  , Moon (V3 3 5 (-1)) 0
  ]
