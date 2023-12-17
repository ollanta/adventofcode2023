import Text.Parsec
import Parsing
import Chart2d
import qualified Data.HashMap.Strict as M
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[Integer]]
readD = many1 digitAsNumber `sepEndBy` newline

solve input = unlines [
  showMS $ inpchart
  , showMC $ M.map (head . show) solpath
  , show $ answer
  ]
  where
    solution = find inpchart
    answer = fst solution
    solpath = M.fromList $ map (\c -> (c, inpchart M.! c)) (snd solution)

    inpchart = readM input
    (mx, my) = maximum . M.keys $ inpchart

    find chart = minsearch heapinit meminit
      where
        heapinit :: H.MinPrioHeap Integer (Coord, Integer, Char, [Coord])
        heapinit = H.fromList . zip (repeat 0) $ [((0,0),0,'R',[])]

        meminit = M.empty

        end c = c == (mx,my)

        moves (c, n, d, p) = [(c', n', d', c:p) | (n', d') <- (n+1, d):zip (repeat 1) turns,
                                                  let c' = move c d',
                                                  inChart c',
                                                  n' <= 3]
          where
            turns
              | d `elem` "LR" = "UD"
              | d `elem` "UD" = "LR"

            move (x,y) 'L' = (x-1,y)
            move (x,y) 'R' = (x+1,y)
            move (x,y) 'U' = (x,y-1)
            move (x,y) 'D' = (x,y+1)

            inChart (x,y) = x >= 0 && x <= mx && y >= 0 && y <= my

        minsearch heap mem
          | end coord    = (heat, reverse $ coord:path)
          | inMemo state = minsearch heap' mem
          | otherwise    = minsearch heap'' mem'
          where
            Just (next, heap') = H.view heap
            (heat, state) = next
            (coord, n, dir, path) = state

            inMemo (c, n, d, _) = (c, n, d) `M.member` mem
            mem' = M.insert (coord, n, dir) True mem

            newStates = [state' | state'@(c, _, _, p) <- moves state,
                                  not (inMemo state')]
            newHeats = [heat + chart M.! c | (c, _, _, _) <- newStates]

            heap'' = H.union heap' . H.fromList $ zip newHeats newStates
