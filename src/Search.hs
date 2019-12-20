module Search
  ( breadthFirst
  ) where

import           Data.Sequence (Seq ((:<|)), (><))
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set

breadthFirst :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
breadthFirst rep reachable start =
  go (Set.singleton $ rep start) (Seq.singleton start)
  where
    go _ Seq.Empty = []
    go seen (x :<| xs) =
      x :
      go (Set.union (Set.fromList $ map rep ys) seen) (xs >< Seq.fromList ys)
      where
        ys = filter (\y -> Set.notMember (rep y) seen) $ reachable x
