{-# LANGUAGE TupleSections #-}

module Day05 where

import           Control.Monad.RWS
import           Data.List.Split
import           Data.Vector           (Vector, fromList, (!), (//))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

type Memory = Vector Int

data Mode
  = Position
  | Immediate
  deriving (Show, Eq)

data Parameter =
  Parameter Mode Int
  deriving (Show, Eq)

data Instruction
  = Add Parameter Parameter Parameter
  | Multiply Parameter Parameter Parameter
  | Receive Parameter
  | Send Parameter
  | JumpIfTrue Parameter Parameter
  | JumpIfFalse Parameter Parameter
  | LessThan Parameter Parameter Parameter
  | Equal Parameter Parameter Parameter
  | Halt
  deriving (Show, Eq)

parse :: String -> Memory
parse = fromList . map read . splitOn ","

instruction :: Int -> Int -> Instruction
instruction i n =
  case n `mod` 100 of
    1  -> Add (param n 1) (param n 2) (param n 3)
    2  -> Multiply (param n 1) (param n 2) (param n 3)
    3  -> Receive (param n 1)
    4  -> Send (param n 1)
    5  -> JumpIfTrue (param n 1) (param n 2)
    6  -> JumpIfFalse (param n 1) (param n 2)
    7  -> LessThan (param n 1) (param n 2) (param n 3)
    8  -> Equal (param n 1) (param n 2) (param n 3)
    99 -> Halt
  where
    param n m =
      case n `div` (10 ^ (m + 1)) `mod` 10 of
        0 -> Parameter Position (i + m)
        1 -> Parameter Immediate (i + m)

fetchVal :: Parameter -> Memory -> Int
fetchVal p m = m ! fetchAddr p m

fetchAddr :: Parameter -> Memory -> Int
fetchAddr (Parameter Position i) m  = m ! i
fetchAddr (Parameter Immediate i) _ = i

run' :: RWS Int [Int] (Int, Memory) ()
run' = do
  (i, m) <- get
  case instruction i $ m ! i of
    Add p1 p2 p3 -> do
      put (i + 4, m // [(fetchAddr p3 m, fetchVal p1 m + fetchVal p2 m)])
      run'
    Multiply p1 p2 p3 -> do
      put (i + 4, m // [(fetchAddr p3 m, fetchVal p1 m * fetchVal p2 m)])
      run'
    Receive p1 -> do
      input <- ask
      put (i + 2, m // [(fetchAddr p1 m, input)])
      run'
    Send p1 -> do
      tell [fetchVal p1 m]
      put (i + 2, m)
      run'
    JumpIfTrue p1 p2 -> do
      put
        ( if fetchVal p1 m /= 0
            then fetchVal p2 m
            else i + 3
        , m)
      run'
    JumpIfFalse p1 p2 -> do
      put
        ( if fetchVal p1 m == 0
            then fetchVal p2 m
            else i + 3
        , m)
      run'
    LessThan p1 p2 p3 -> do
      put
        ( i + 4
        , m //
          [ ( fetchAddr p3 m
            , if fetchVal p1 m < fetchVal p2 m
                then 1
                else 0)
          ])
      run'
    Equal p1 p2 p3 -> do
      put
        ( i + 4
        , m //
          [ ( fetchAddr p3 m
            , if fetchVal p1 m == fetchVal p2 m
                then 1
                else 0)
          ])
      run'
    Halt -> return ()

run :: Int -> Memory -> [Int]
run i = snd . execRWS run' i . (0, )

memory :: IO Memory
memory = parse <$> readFile "data/input05"

part1 :: IO Int
part1 = last . run 1 <$> memory

part2 :: IO Int
part2 = last . run 5 <$> memory

test =
  defaultMain $
  testGroup
    "day05"
    [ testCase "diagnostics" $ do
        out <- run 1 <$> memory
        init out @?= [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , testProperty "run 3,0,4,0,99" $ \x -> run x (parse "3,0,4,0,99") === [x]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $ \x ->
        run x (parse "3,9,8,9,10,9,4,9,99,-1,8") ===
        [ if x == 8
            then 1
            else 0
        ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $ \x ->
        run x (parse "3,9,7,9,10,9,4,9,99,-1,8") ===
        [ if x < 8
            then 1
            else 0
        ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $ \x ->
        run x (parse "3,3,1108,-1,8,3,4,3,99") ===
        [ if x == 8
            then 1
            else 0
        ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $ \x ->
        run x (parse "3,3,1107,-1,8,3,4,3,99") ===
        [ if x < 8
            then 1
            else 0
        ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $ \x ->
        run x (parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") ===
        [ if x /= 0
            then 1
            else 0
        ]
    , testProperty "run 3,9,8,9,10,9,4,9,99,-1,8" $ \x ->
        run x (parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") ===
        [ if x /= 0
            then 1
            else 0
        ]
    , testProperty "sample input" $
      idempotentIOProperty $ do
        p <- parse <$> readFile "data/test05"
        return $ \x ->
          run x p ===
          [ case compare x 8 of
              LT -> 999
              EQ -> 1000
              GT -> 1001
          ]
    , testCase "part1" $ do
        p1 <- part1
        p1 @?= 14155342
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 8684145
    ]
