{-# LANGUAGE RecordWildCards #-}

module Day23 where

import           Control.Monad
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Sequence    (Seq (..))
import qualified Data.Sequence    as Seq
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Intcode

data Packet =
  Packet
    { address :: Int
    , x       :: Int
    , y       :: Int
    }
  deriving (Show)

data NetworkedComputer =
  NetworkedComputer
    { computer :: Intcode.Effect
    , inbox    :: Seq Packet
    }

type Network = Map Int NetworkedComputer

network :: Intcode.Program -> Network
network p =
  Map.fromList
    [ ( i
      , NetworkedComputer (Intcode.expectInput (Intcode.runDynamic p) i) Empty)
    | i <- [0 .. 49]
    ]

stepComputer :: NetworkedComputer -> (NetworkedComputer, Maybe Packet)
stepComputer NetworkedComputer {..} =
  case computer of
    Intcode.Input f ->
      case inbox of
        Packet {..} :<| ps ->
          (NetworkedComputer (Intcode.expectInput (f x) y) ps, Nothing)
        Empty -> (NetworkedComputer (f (-1)) Empty, Nothing)
    Intcode.Output address (Intcode.Output x (Intcode.Output y c)) ->
      (NetworkedComputer c inbox, Just $ Packet address x y)

stepNetwork :: Network -> (Network, [Packet])
stepNetwork = Map.foldrWithKey f (Map.empty, [])
  where
    f i computer (nw, packets) =
      case stepComputer computer of
        (c, Just packet) -> (Map.insert i c nw, packet : packets)
        (c, Nothing)     -> (Map.insert i c nw, packets)

-- Either deliver the packet successfully, or fail if a packet is addressed to a
-- computer that doesn't exist.
deliverPacket :: Network -> Packet -> Either Packet Network
deliverPacket nw packet
  | Map.member (address packet) nw =
    Right $
    Map.adjust (\nwc -> nwc {inbox = inbox nwc :|> packet}) (address packet) nw
  | otherwise = Left packet

step :: Network -> Either Packet Network
step = uncurry (foldM deliverPacket) . stepNetwork

part1' :: Intcode.Program -> Int
part1' p = go $ network p
  where
    go nw =
      case step nw of
        Left (Packet 255 _ y) -> y
        Right nw'             -> go nw'

part1 :: IO Int
part1 = part1' . Intcode.parse <$> readFile "data/input23"

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day23"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 17541
    ]
