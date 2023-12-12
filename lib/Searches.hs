module Searches where

import qualified Data.HashSet as S
import Data.Hashable

dfs :: (Hashable a) => (a,s) -> ((a,s) -> [(a,s)]) -> ((a,s) -> Bool) -> (Maybe (a,s), S.HashSet a)
dfs startNode getNeighbours finished = dfs' startNode S.empty
  where
    dfs' t@(n,s) mem
      | finished t  = (Just t, mem')
      | null neighs = (Nothing, mem')
      | otherwise   = helper mem' neighs
      where
        mem' = S.insert n mem
        neighs = filter (not . (`S.member` mem') . fst) $ getNeighbours t

        helper m (t:ts) = case dfs' t m of
          (Just t, m')  -> (Just t, m')
          (Nothing, m') -> helper m' ts
        helper m [] = (Nothing, m)

basicDfs :: (Hashable a) => a -> (a -> [a]) -> (a -> Bool) -> Maybe [a]
basicDfs startNode getNeighbours finished = fmap (reverse . snd) . fst $ dfs startNode' getNeighbours' finished'
  where
    startNode' = (startNode, [startNode])
    getNeighbours' (n, s) = [(n', n':s) | n' <- getNeighbours n]
    finished' = finished . fst

floodFill startNode getNeighbours = S.toList mem
  where
    startNode' = (startNode, ())
    getNeighbours' (n, _) = [(n', ()) | n' <- getNeighbours n]
    finished = const False

    (Nothing, mem) = dfs startNode' getNeighbours' finished