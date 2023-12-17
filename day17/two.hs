import Text.Parsec
import Parsing
import Chart2d
import qualified Data.HashMap.Strict as M
import Searches
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
        heapinit = H.fromList . zip (repeat 0) $ [((0,0),10,'R',[]),((0,0),10,'D',[])]

        meminit = M.empty

        end ((x, y),_,_) = (x,y) == (mx,my)

        moves (c,n,d)
          | n < 4 = filter inChart [(moves' c d, n+1, d)]
          | otherwise = filter inChart [(moves' c d', newN n d', d') | d' <- newDs d, newN n d' <= 10]
          where
            newDs 'L' = "UDL"
            newDs 'R' = "UDR"
            newDs 'U' = "LRU"
            newDs 'D' = "LRD"

            newN n d'
              | d == d' = n + 1
              | otherwise = 1

            moves' (x,y) 'L' = (x-1,y)
            moves' (x,y) 'R' = (x+1,y)
            moves' (x,y) 'U' = (x,y-1)
            moves' (x,y) 'D' = (x,y+1)

            inChart ((x,y),_,_) = x >= 0 && x <= mx && y >= 0 && y <= my

        minsearch heap mem
          | end state = (heat, coord:path)
          | state `M.member` mem = minsearch heap' mem
          | otherwise = minsearch heap'' mem'
          where
            Just (next, heap') = H.view heap
            (heat, lstate) = next
            (coord, n, d, path) = lstate
            state = (coord, n, d)

            mem' = M.insert state True mem

            newStates = filter (not . (`M.member` mem)) $ moves state

            newHeats = [heat + chart M.! c | (c,_,_) <- newStates]

            newLstates = [(a,b,c,coord:path) | (a,b,c) <- newStates]

            heap'' = H.union heap' $ H.fromList (zip newHeats newLstates)
