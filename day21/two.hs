import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD = many1 (oneOf ".#S") `sepEndBy` newline

solve input = unlines [
  showMC $ inputM
  ] ++ unlines (map show $ answer)
  where
    answer = map (allFrom startNode) $ [880..900] ++ [totalSteps]

    inputM = readM input

    totalSteps = 26501365

    -- assuming start in the middle, all clear paths NSEW from start, square grid, odd size
    allFrom coord@(sx,sy) steps = center + sides + corners
      where
        center = solveFrom coord steps

        -- cases where the tiles are entered from the middle after crossing n x tiles + half a tile (from start)
        -- (straight left / right / up / down)
        sdeltas = takeWhile (<= totalSteps) [(sx+1)+n*(maxX+1) | n <- [0..]] -- really maxX+1 and maxX `div` 2, but startcoord matters
        sides = sum [helper (repeat 1) sdeltas (solveFrom entrance) | entrance <- [(maxX, sy), (0, sy), (sx, maxY), (sx, 0)]]

        -- cases where the tiles are entered from the corner after crossing n x tiles + half a tile (from start)
        cdeltas = map (sy+1+) sdeltas
        corners = sum [helper [1..] cdeltas (solveFrom entrance) | entrance <- [(maxX, maxY), (0, maxY), (maxX,0), (0,0)]]
        
        -- count the number of possible positions (counted by solver), where solver x only depends on if x is even/odd for larger x
        helper ks deltas solver = sum [solver' k d | (k, d) <- zip ks deltas]
          where
            solver' k d
              | steps-d <= (maxX+1)*2 = k*solver (steps-d)
              | d `rem` 2 == 0 = k*even
              | otherwise      = k*odd

            even = solver steps
            odd = solver (steps-1)


    startNode :: Coord
    startNode = head . M.keys $ M.filter (=='S') inputM
    (maxX, maxY) = maximum . M.keys $ inputM

    -- solve for a single tile, entered from a coord, the step-by-step fill, and the repeated cycle
    solveFrom :: Coord -> Integer -> Integer
    solveFrom coord steps
      | steps < 0 = 0
      | steps < repeatsAfter              = getPositions $ prefix !! fromInteger steps
      | (steps-repeatsAfter) `rem` 2 == 0 = getPositions even
      | otherwise                         = getPositions odd
      where
        stepbystep = bfs S.empty (S.singleton coord) inputM [coord]
        (prefix, even:odd:repeats) = span (\(_,_,n) -> n > 0) stepbystep
        repeatsAfter = toInteger $ length prefix

        getPositions (this, _, _) = toInteger $ S.size this

    -- bfs memoizing the cyclic structure of even/odd moves
    bfs mprev mthis chart nexts = (mthis, mprev, length nexts'): bfs mprev' mthis' chart nexts'
      where
        nexts' :: [Coord]
        nexts' = S.toList nextsm'
        nextsm = S.fromList $ concatMap neighbours nexts
        nextsm' :: S.HashSet Coord
        nextsm' = S.filter (\n -> good n && not (S.member n mprev)) nextsm

        mthis' = S.union mprev nextsm'
        mprev' = mthis

        good neighbour
          | here == Just '.' = True
          | here == Just 'S' = True
          | otherwise        = False
          where
            here = chart M.!? neighbour