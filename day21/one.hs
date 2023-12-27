import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d

main :: IO ()
main = optimisticInteract readD solve

readD = many1 (oneOf ".#S") `sepEndBy` newline

solve input = unlines [
  showMC $ inputM
  , show $ answer
  ]
  where
    answer = length $ bfs 64 inputM startNodes

    inputM = readM input

    startNodes = M.keys $ M.filter (=='S') inputM

    bfs 0 _ nexts = nexts
    bfs steps chart nexts = bfs (steps-1) chart nexts'
      where
        nexts' = nub [neighbour | neighbour <- concatMap neighbours nexts, good neighbour]

        good neighbour
          | here == Just '.' = True
          | here == Just 'S' = True
          | otherwise        = False
          where
            here = chart M.!? neighbour