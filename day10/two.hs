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
  , showMC chart
  , show v
  , show answer
  ]
  where
    answer = length $ findClosure loop (mapStart chart start)
    (v, loop) = fillFrom [start] 0 M.empty chart

    mapStart ch s@(x,y) = M.insert s s' ch
      where
        s'
          | ns == [(x,y-1),(x,y+1)] = '|'
          | ns == [(x-1,y),(x+1,y)] = '-'
          | ns == [(x+1,y),(x,y-1)] = 'L'
          | ns == [(x-1,y),(x,y-1)] = 'J'
          | ns == [(x-1,y),(x,y+1)] = '7'
          | ns == [(x+1,y),(x,y+1)] = 'F'
          where
            ns = filter (`M.member` loop) $ pipenext chart s

    chart = readM lines
    start = head . M.keys $ M.filter (=='S') chart

    fillFrom cs v mem ch
      | null cs' = (v,mem)
      | otherwise = fillFrom cs' (v+1) mem' ch
      where
        cs' = filter (not . (`M.member` mem)) $ concatMap (pipenext ch) cs
        mem' = M.union mem . M.fromList $ zip cs' (repeat True)
            
    pipenext ch c = [nx | nx <- next c, any (==c) $ next nx]
      where
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

    findClosure loop ch = concatMap (helper False 0) [y | y <- [0..maxy]]
      where
        (maxx, maxy) = maximum $ M.keys ch

        helper _ x _
          | x > maxx = []
        helper b x y
          | not inLoop && b = (x,y) : helper b (x+1) y
          | not inLoop      = helper b (x+1) y
          where
            inLoop = M.member (x,y) loop
        helper b x y
          | here == 'L' && there == 'J' = helper b (x'+1) y
          | here == 'L' && there == '7' = helper (not b) (x'+1) y
          | here == 'F' && there == '7' = helper b (x'+1) y
          | here == 'F' && there == 'J' = helper (not b) (x'+1) y
          | here == '|' = helper (not b) (x+1) y
          where
            here = ch M.! (x,y)
            there = ch M.! (x',y)
            x' = head . dropWhile (\x' -> ch M.! (x',y) == '-') $ [x+1..maxx]