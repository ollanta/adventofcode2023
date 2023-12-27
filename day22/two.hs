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
    answer = sum . map ((\k -> k-1) . length . falls) $ M.keys inpM
      where
        inp' = zip ([1..] :: [Integer]) input
        inpM = M.fromList inp'
        stack = run inp'

        falls ci = falls' (M.filterWithKey (\k v -> k /= ci) restsOnM) [ci]
          where
            falls' m [] = []
            falls' m cs = cs ++ falls' m'' cs'
              where
                m' = M.map (filter (not . (`elem` cs))) m
                cs' = M.keys $ M.filter null m'
                m'' = M.difference m' . M.fromList $ zip cs' (repeat [])

            restsOnM = M.fromList [(c, restsOn c) | c <- M.keys inpM]
            restsOn c = nub . filter (/=c) $ concat [map fst . filter ((==z-1) . snd) $ stack M.! (x,y) | let ((x1,y1,_),(x2,y2,_)) = inpM M.! c, x <- [x1..x2], y <- [y1..y2], let (_,z) = minimum . filter ((==c) . fst) $ stack M.! (x,y)]

    run :: [(Integer, ((Integer, Integer, Integer), (Integer, Integer, Integer)))] -> M.HashMap (Integer, Integer) [(Integer, Integer)]
    run inp = helper initSettled inp
      where
        xycoords = [(x,y) | (_,((x1,y1,_), (x2,y2,_)))  <- inp, x <- [x1..x2], y <- [y1..y2]]
        (minX, minY) = minimum xycoords
        (maxX, maxY) = maximum xycoords
        initSettled = M.fromList [((x,y),[(0,0)]) | x <- [minX..maxX], y <- [minY..maxY]]

        mindrop settled brick = minimum [z1 - maximum (filter (<z1) . map snd $ settled M.! (x,y)) | let (_, ((x1,y1,z1), (x2,y2,_))) = brick, x <- [x1..x2], y <- [y1..y2]]

        helper settled [] = settled
        helper settled bricks
          | null toSettle = helper settled' rest'
          | otherwise     = helper settled' rest
          where
            (toSettle, rest) = partition ((==1) . (mindrop settled)) $ bricks
            settled' = M.unionWith (++) settled $ M.fromListWith (++) [((x,y), [(name, bz)]) | (name, ((x1,y1,z1), (x2,y2,z2))) <- toSettle, x <- [x1..x2], y <- [y1..y2], bz <- nub [z1, z2]]
            rest' = [(name,((x1,y1,z1-1),(x2,y2,z2-1))) | (name,((x1,y1,z1),(x2,y2,z2))) <- rest]
