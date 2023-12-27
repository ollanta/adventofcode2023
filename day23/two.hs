import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d
import Data.Maybe

main :: IO ()
main = optimisticInteract readD solve

readD = many1 (oneOf ".#><v^") `sepEndBy` newline


solve input = unlines [
  showMC inpm
  , show $ answer
  ]
  where
    inpm = readM input
    answer = getPaths

    getPaths = (maximum $ map (sum . map snd . M.elems) genNodePaths, M.size nodes)
      where
        (maxX, maxY) = maximum . M.keys $ inpm
        [start] = [(x,0) | x <- [0..maxX], inpm M.! (x,0) == '.']
        [end] = [(x,maxY) | x <- [0..maxX], inpm M.! (x,maxY) == '.']

        nodes = genNodes start

        genNodePaths = helper [(M.empty, start)]
          where
            helper [] = []
            helper ((m, s):mss)
              | s == end  = m : helper mss
              | otherwise = helper (mss' ++ mss)
              where
                mss' = [(M.insert n edge m, n) | edge@(n, _) <- nodes M.! s, not (n `M.member` m)]
        
        genNodes s = helper M.empty [s]
          where
            helper m [] = m
            helper m (n:ns)
              | n `M.member` m = helper m ns
              | otherwise      = helper m' (ns ++ map fst edges)
              where
                edges = genPaths n
                m' = M.insert n edges m

            genPaths n = catMaybes [genPath [n] n' | n' <- movesFrom n]

            genPath :: [Coord] -> Coord -> Maybe (Coord, Int)
            genPath path n
              | n == end       = Just (n, length path)
              | length new > 1 = Just (n, length path)
              | null new       = Nothing
              | otherwise      = genPath (n:path) n'
              where
                new = [n' | n' <- movesFrom n, not (n' `elem` path)]
                [n'] = new


    movesFrom coord@(x,y) = [n | n <- neighbours coord, not (inpm M.!? n `elem` [Nothing, Just '#'])]
