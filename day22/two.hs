import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD = readLine `sepEndBy` newline
  where
    readLine = do
      c1 <- readCoord
      char '~'
      c2 <- readCoord
      return (c1, c2)

    readCoord = do
      n1 <- number
      char ','
      n2 <- number
      char ','
      n3 <- number
      return (n1, n2, n3)


solve input = unlines [
  show 1
  , show $ answer
  ]
  where
    answer = sum . map (length . falls . fst) $ keyedInput
      where
        keyedInput = zip [1..] input
        stack = run keyedInput

        falls ci = falls' restsOnM [ci]
          where
            falls' m [] = []
            falls' m cs = cs' ++ falls' m'' cs'
              where
                m' = M.map (filter (not . (`elem` cs))) m
                m'' = M.filter (not . null) m'
                cs' = M.keys $ M.difference m' m''

            restsOnM = M.map nub $ M.fromListWith (++) [(a, [b]) | xystack <- M.elems stack, (a, b) <- restsOnInStack xystack]
            restsOnInStack xystack = [(n2, n1) | ((n1, z1), (n2, z2)) <- zip sortedStack (tail sortedStack), n1 /= n2, z1+1 == z2]
              where
                sortedStack = sortOn snd $ filter ((/=0) . fst) xystack

    run :: [(Int, ((Integer, Integer, Integer), (Integer, Integer, Integer)))] -> M.HashMap (Integer, Integer) [(Int, Integer)]
    run inp = settleBricks initSettled inp
      where
        xycoords = [(x,y) | (_,((x1,y1,_), (x2,y2,_)))  <- inp, x <- [x1..x2], y <- [y1..y2]]
        initSettled = M.fromList $ zip xycoords (repeat [(0,0)])

        mindrop settled brick = z1 - maximum settledBelow
          where
            (_, ((x1, y1, z1), (x2, y2, _))) = brick
            settledBelow = [z | x <- [x1..x2], y <- [y1..y2], let xystack = settled M.! (x,y), (_, z) <- xystack, z < z1]

        settleBricks settled [] = settled
        settleBricks settled bricks
          | null toSettle = settleBricks settled' rest'
          | otherwise     = settleBricks settled' rest
          where
            (toSettle, rest) = partition ((==1) . (mindrop settled)) $ bricks
            settled' = M.unionWith (++) settled $ M.fromListWith (++) [((x,y), [(name, bz)]) | (name, ((x1,y1,z1), (x2,y2,z2))) <- toSettle, x <- [x1..x2], y <- [y1..y2], bz <- nub [z1, z2]]
            rest' = [(name,((x1,y1,z1-1),(x2,y2,z2-1))) | (name,((x1,y1,z1),(x2,y2,z2))) <- rest]
