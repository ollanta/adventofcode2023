import Data.Char
import Text.Parsec
import Parsing
import Data.List
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser (String, [(String, (String, String))])
readD = readAll
  where
    readAll = do
      rl <- word
      many1 newline
      nodes <- readNode `sepEndBy` newline
      return (rl, nodes)

    readNode = do
      node <- many1 alphaNum
      string " = ("
      left <- many1 alphaNum
      string ", "
      right <- many1 alphaNum
      string ")"
      return (node, (left, right))


solve (directions, graphAsList) = unlines [
  show $ [key | key <- M.keys graph, endsWith key 'A']
  , show $ map findCycle paths
  , show $ map (take 10 . findZs) paths
  , show $ map (take 10 . allZs) paths
  , show answer
  ]
  where
    graph = M.fromList graphAsList
    traverse n d
      | d == 'L' = fst $ graph M.! n
      | d == 'R' = snd $ graph M.! n

    paths = [scanl traverse start (cycle directions) | start <- [key | key <- M.keys graph, endsWith key 'A']]

    findCycle path = helper M.empty $ zip3 [0..] (cycle [1..length directions]) path
      where
        helper mem ((i, ci, n):ts)
          | (ci, n) `M.member` mem = (mem M.! (ci, n), i)
          | otherwise              = helper (M.insert (ci, n) (i, ci, n) mem) ts

    findZs path = [i | (i, n) <- zip [0..] path, endsWith n 'Z']

    allZs path = concat . iterate (map (\zi->zi+ri-oi)) $ zs
      where
        ((oi, _, _), ri) = findCycle path
        zs = findZs (take ri $ path)

    findMatch ls
      | all (==maxHead) heads = maxHead
      | otherwise = findMatch . map (dropWhile (< maxHead)) $ ls
      where
        heads = map head ls
        maxHead = maximum heads

    answer = findMatch $ map allZs paths

    endsWith s c = last s == c