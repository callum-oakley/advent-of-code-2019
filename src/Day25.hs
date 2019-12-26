module Day25 where

import           Data.Char

import qualified Intcode

input :: String
input =
  unlines
    [ "west"
    , "west"
    , "north"
    , "take space heater"
    , "south"
    , "east"
    , "south"
    , "south"
    , "take sand"
    , "north"
    , "north"
    , "east"
    , "east"
    , "take mug"
    , "east"
    , "south"
    , "east"
    , "south"
    , "take easter egg"
    , "north"
    , "west"
    , "west"
    , "south"
    , "west"
    , "south"
    , "south"
    ]

part1 :: IO ()
part1 = do
  p <- Intcode.parse <$> readFile "data/input25"
  go (Intcode.runDynamic p) input
  where
    go (Intcode.Input f) (x:xs) = putStr [x] >> go (f $ ord x) xs
    go (Intcode.Input f) []     = getChar >>= (\c -> go (f $ ord c) [])
    go (Intcode.Output o c') xs = putStr (display o) >> go c' xs
    go Intcode.Stop _           = pure ()
    display o
      | o < 128 = [chr o]
      | otherwise = show o
