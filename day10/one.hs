import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[Char]]
readD = (many1 $ oneOf "|-LJ7F.S") `sepEndBy` newline


solve lines = unlines [
  show $ lines
  , show start
  , show answer
  ]
  where
    answer = fillFrom [start] 0 M.empty chart

    chart = readM lines
    start = head . M.keys $ M.filter (=='S') chart

    fillFrom cs v mem ch
      | null cs' = v
      | otherwise = fillFrom cs' (v+1) mem' ch
      where
        cs' = concatMap pipenext cs
        mem' = M.union mem . M.fromList $ zip cs' (repeat True)
            
        pipenext c = [nx | nx <- next c, any (==c) $ next nx, not $ nx `M.member` mem]

        next c@(x,y)
          | here == '.' = []
          | here == '|' = [(x,y-1),(x,y+1)]
          | here == '-' = [(x-1,y),(x+1,y)]
          | here == 'L' = [(x,y-1),(x+1,y)]
          | here == 'J' = [(x,y-1),(x-1,y)]
          | here == '7' = [(x-1,y),(x,y+1)]
          | here == 'F' = [(x,y+1),(x+1,y)]
          | here == 'S' = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
          where
            here = ch M.! c

