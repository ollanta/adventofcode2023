import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List
import Searches
import Chart2d
import Data.Maybe
import qualified Linear.Matrix as X
import Linear.V2

main :: IO ()
main = optimisticInteract readD solve

readD = readLine `sepEndBy` newline
  where
    readLine = do
      s1 <- many1 letter
      string ": "
      ss <- many1 letter `sepBy` char ' '
      return (s1, ss)

solve input = unlines [
  show $ edgesToCut
  , show $ answer
  ]
  where
    answer = (g1, g2, g1*g2)
      where
        (n1, n2) = head edgesToCut
        graph' = foldr removeEdge inpM edgesToCut
        [g1, g2] = map (groupsize graph') [n1, n2]

    removeEdge (n1, n2) graph = M.adjust (filter (/=n2)) n1 . M.adjust (filter (/=n1)) n2 $ graph

    edgesToCut = edgeTrio
      where
        -- simplifying assumption that the edges to cut are between the two nodes with lowest combined radius
        withMaxD = M.mapWithKey (\k _ -> maxD k) inpM
        minMaxD = minimum $ M.elems withMaxD
        centerNodes = M.keys . M.filter (==minMaxD) $ withMaxD

        combinations [] = [[]]
        combinations (n:ns) = [(min n n2, max n n2) : nscomb | n2 <- inpM M.! n, nscomb <- combinations ns]
        [edgeTrio] = filter cleansplit . filter ((==3) . length) . map nub $ combinations centerNodes

        cleansplit cutEdges@((n1,n2):_) = groupsize graph' n1 /= toInteger (M.size inpM)
          where
            graph' = foldr removeEdge inpM cutEdges

    inpM = M.map nub combined
      where
        onto = M.fromList input
        back = M.fromListWith (++) [(v,[k]) | (k,vs) <- M.toList onto, v <- vs]
        combined = M.unionWith (++) onto back

    groupsize :: M.HashMap String [String] -> String -> Integer
    groupsize graph start = toInteger . length $ floodFill start (graph M.!)

    maxD :: String -> Integer
    maxD start = bfs S.empty [start] 0
      where
        bfs s []    k = k
        bfs s nodes k = bfs s' nodes' (k+1)
          where
            s' = S.union s $ S.fromList nodes
            nodes' = [next | node <- nodes, next <- inpM M.! node, not (next `S.member` s')]