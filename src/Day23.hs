{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Day23 where

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

data NAT =
  NAT
    { current :: Maybe Packet
    , sent    :: [Packet]
    }

data Network =
  Network
    { computers :: Map Int NetworkedComputer
    , nat       :: NAT
    }

network :: Intcode.Program -> Network
network p =
  Network
    (Map.fromList
       [ ( i
         , NetworkedComputer
             (Intcode.expectInput (Intcode.runDynamic p) i)
             Empty)
       | i <- [0 .. 49]
       ])
    (NAT Nothing [])

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
stepNetwork Network {..} = (Network {computers = computers', nat}, packets')
  where
    (computers', packets') = Map.foldrWithKey f (Map.empty, []) computers
    f i computer (nw, packets) =
      case stepComputer computer of
        (c, Just packet) -> (Map.insert i c nw, packet : packets)
        (c, Nothing)     -> (Map.insert i c nw, packets)

deliverPacket :: Network -> Packet -> Network
deliverPacket Network {..} packet
  | address packet == 255 =
    Network {computers, nat = nat {current = Just packet}}
  | otherwise =
    Network
      { computers =
          Map.adjust
            (\nwc -> nwc {inbox = inbox nwc :|> packet})
            (address packet)
            computers
      , nat
      }

isIdle :: Network -> Bool
isIdle =
  all (\NetworkedComputer {..} -> Seq.null inbox && Intcode.isInput computer) .
  computers

wake :: Network -> Network
wake Network {..} =
  Network
    { computers =
        Map.adjust (\nwc -> nwc {inbox = inbox nwc :|> packet}) 0 computers
    , nat = nat {sent = packet : sent nat}
    }
  where
    Just packet = current nat

step :: Network -> Network
step nw
  | isIdle nw' = wake nw'
  | otherwise = nw'
  where
    nw' = uncurry (foldl deliverPacket) $ stepNetwork nw

part1' :: Intcode.Program -> Int
part1' p = go $ network p
  where
    go nw =
      case current $ nat nw of
        Just Packet {..} -> y
        Nothing          -> go $ step nw

part2' :: Intcode.Program -> Int
part2' p = go $ network p
  where
    go nw =
      case sent $ nat nw of
        Packet _ _ y0:Packet _ _ y1:_
          | y0 == y1 -> y0
        _ -> go $ step nw

part1 :: IO Int
part1 = part1' . Intcode.parse <$> readFile "data/input23"

part2 :: IO Int
part2 = part2' . Intcode.parse <$> readFile "data/input23"

test :: IO ()
test = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "day23"
    [ testCase "part1" $ do
        p1 <- part1
        p1 @?= 17541
    , testCase "part2" $ do
        p2 <- part2
        p2 @?= 12415
    ]
