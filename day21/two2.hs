import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD = many1 (oneOf ".#S") `sepEndBy` newline

{-
This solution is too slow for the -real- solution, but fairly fast to debug large step numbers
-}

solve input = unlines [
  showMC $ inputM
  ] ++ unlines (map show $ answer)
  where
    targetSteps = [880..900]
    answer = filter ((`elem` targetSteps) . fst) . zip [0..] $ map debugOutput iters
      where
        iters = bfs M.empty startnodesM (maximum targetSteps + 1) inputM startNodes

        debugOutput (mt, mp, s) = (interesting mt, interesting mp, s)
        interesting m = sum [S.size $ m M.! (tx,ty) | (tx,ty) <- M.keys m] -- add filter to tx/ty as fits

        startnodesM :: M.HashMap Coord (S.HashSet Coord)
        startnodesM = M.fromList [(tc, S.singleton c) | (tc, c) <- startNodes]

    inputM = readM input

    startNodes :: [(Coord, Coord)]
    startNodes = zip [(0,0)] . M.keys $ M.filter (=='S') inputM

    -- bfs memoizing the cyclic structure of even/odd moves
    bfs mprev mthis 0 _ nexts = [(mthis, mprev, 0)]
    bfs mprev mthis steps chart nexts = (mthis, mprev, length nexts'): bfs mprev' mthis' (steps-1) chart nexts'
      where
        nexts' :: [(Coord,Coord)]
        nexts' = [(tc, c) | (tc, s) <- M.toList nextsm', c <- S.toList s]
        nextsm = M.fromListWith S.union $ [(tc, S.singleton c) | (tc, c) <- concatMap allNeighbours nexts]
        nextsm' :: M.HashMap Coord (S.HashSet Coord)
        nextsm' = M.mapWithKey (\tc set -> S.filter (\n -> good n && not (S.member n $ M.findWithDefault S.empty tc mprev)) set) nextsm

        mthis' = M.unionWith S.union mprev nextsm'
        mprev' = mthis

        (mx, my) = maximum . M.keys $ chart
        allNeighbours (tc, next) = [fixTc (tc, neighbour) | neighbour <- neighbours next]
          where
            fixTc ((tx, ty), (x,y))
              | x == -1   = ((tx-1, ty), (mx, y))
              | x == mx+1 = ((tx+1, ty), (0, y))
              | y == -1   = ((tx, ty-1), (x, my))
              | y == my+1 = ((tx, ty+1), (x, 0))
              | otherwise = ((tx, ty), (x,y))

        good neighbour
          | here == Just '#' = False
          | otherwise        = True
          where
            here = chart M.!? neighbour