module Day14 where

import           Data.Foldable
import           Data.List.Split
import           Data.Map.Strict  (Map, (!))
import qualified Data.Map.Strict  as Map
import           Test.Tasty
import           Test.Tasty.HUnit

type Chemical = String

type Reactions = Map Chemical (Map Chemical Int)

type Lab = Map Chemical Int

parse :: String -> Reactions
parse =
  Map.fromList .
  map
    ((\((c, n), r) -> (c, Map.insert c (-n) r)) . parseReaction . splitOn " => ") .
  lines
  where
    parseReaction [reactants, products] =
      ( parseChemical products
      , Map.fromList . map parseChemical $ splitOn ", " reactants)
    parseChemical = (\[n, chemical] -> (chemical, read n)) . words

reduce :: Reactions -> Lab -> Lab
reduce rs lab =
  case find (\(_, n) -> n > 0) . Map.assocs $ Map.delete "ORE" lab of
    Just (chemical, n) -> reduce rs (lab' n chemical)
    Nothing            -> lab
  where
    lab' n chemical =
      Map.unionWith (+) lab . fmap (* applications n chemical) $ rs ! chemical
    applications n chemical =
      ceiling $ fromIntegral n / fromIntegral (-(rs ! chemical ! chemical))

cost :: Reactions -> Int -> Int
cost rs fuels = reduce rs (Map.singleton "FUEL" fuels) ! "ORE"

part1' :: Reactions -> Int
part1' rs = cost rs 1

binarySearch :: (Int -> Ordering) -> Int -> Int -> Int
binarySearch f low high =
  if mid == low
    then mid
    else case f mid of
           LT -> binarySearch f mid high
           EQ -> mid
           GT -> binarySearch f low mid
  where
    mid = (low + high) `div` 2

part2' :: Reactions -> Int
part2' rs = binarySearch f lowerBound (2 * lowerBound)
  where
    f = flip compare target . cost rs
    lowerBound = target `div` cost rs 1
    target = 1000000000000

reactions :: IO Reactions
reactions = parse <$> readFile "data/input14"

part1 :: IO Int
part1 = part1' <$> reactions

part2 :: IO Int
part2 = part2' <$> reactions

test :: IO ()
test = defaultMain tests

testReactions :: [Reactions]
testReactions =
  map
    (parse . unlines)
    [ [ "10 ORE => 10 A"
      , "1 ORE => 1 B"
      , "7 A, 1 B => 1 C"
      , "7 A, 1 C => 1 D"
      , "7 A, 1 D => 1 E"
      , "7 A, 1 E => 1 FUEL"
      ]
    , [ "9 ORE => 2 A"
      , "8 ORE => 3 B"
      , "7 ORE => 5 C"
      , "3 A, 4 B => 1 AB"
      , "5 B, 7 C => 1 BC"
      , "4 C, 1 A => 1 CA"
      , "2 AB, 3 BC, 4 CA => 1 FUEL"
      ]
    , [ "157 ORE => 5 NZVS"
      , "165 ORE => 6 DCFZ"
      , "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
      , "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
      , "179 ORE => 7 PSHF"
      , "177 ORE => 5 HKGWZ"
      , "7 DCFZ, 7 PSHF => 2 XJWVT"
      , "165 ORE => 2 GPVTF"
      , "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
      ]
    , [ "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
      , "17 NVRVD, 3 JNWZP => 8 VPVL"
      , "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
      , "22 VJHF, 37 MNCFX => 5 FWMGM"
      , "139 ORE => 4 NVRVD"
      , "144 ORE => 7 JNWZP"
      , "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
      , "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
      , "145 ORE => 6 MNCFX"
      , "1 NVRVD => 8 CXFTF"
      , "1 VJHF, 6 MNCFX => 4 RFSQX"
      , "176 ORE => 6 VJHF"
      ]
    , [ "171 ORE => 8 CNZTR"
      , "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
      , "114 ORE => 4 BHXH"
      , "14 VRPVC => 6 BMBT"
      , "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
      , "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
      , "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
      , "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
      , "5 BMBT => 4 WPTQ"
      , "189 ORE => 9 KTJDG"
      , "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
      , "12 VRPVC, 27 CNZTR => 2 XDBXC"
      , "15 KTJDG, 12 BHXH => 5 XCVML"
      , "3 BHXH, 2 VRPVC => 7 MZWV"
      , "121 ORE => 7 VRPVC"
      , "7 XCVML => 6 RJRHP"
      , "5 BHXH, 4 VRPVC => 5 LTCX"
      ]
    ]

tests :: TestTree
tests =
  testGroup
    "day14"
    [ testCase "part1'" . mapM_ (\(rs, expected) -> part1' rs @?= expected) $
      zip testReactions [31, 165, 13312, 180697, 2210736]
    , testCase "part2'" . mapM_ (\(rs, expected) -> part2' rs @?= expected) $
      zip (drop 2 testReactions) [82892753, 5586022, 460664]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 469536
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 3343477
    ]
