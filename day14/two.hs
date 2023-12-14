import Text.Parsec
import Parsing
import Generic
import Data.List
import Chart2d
import qualified Data.HashMap.Strict as M

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [String]
readD = many1 (oneOf ".#O") `sepEndBy` newline

solve input = unlines [
  showMC $ inpchart
  , showMC $ tiltNorth inpchart
  , show $ answer
  ]
  where
    answer = score $ findAt 1000000000 inpchart

    findCycle chart = helper M.empty $ zip [0..] (iterate cycle chart)
      where
        helper mem ((i, ch):rest)
          | strchart `M.member` mem = (mem M.! strchart, i)
          | otherwise = helper (M.insert strchart (ch, i) mem) rest
          where
            strchart = showMC ch

    findAt x chart = head . drop extra $ iterate cycle ch
      where
        ((ch, oi), ni) = findCycle chart
        extra = (x-oi) `rem` (ni-oi)

    inpchart = readM input

    score chart = sum . map scoreRock . rocks $ chart
      where
        (mx,my) = maximum $ M.keys chart
        scoreRock (_,y) = my-y+1

    rotate90 chart = M.mapKeys rot90 chart
      where
        (mx,my) = maximum $ M.keys chart
        rot90 (x,y) = (my-y,x)

    cycle = rotate90 . tiltNorth . rotate90 . tiltNorth . rotate90 . tiltNorth . rotate90 . tiltNorth

    rocks chart = sort . M.keys . M.filter (=='O') $ chart

    tiltNorth chart = helper chart (rocks chart)
      where
        helper ch [] = ch
        helper ch (r@(rx,ry):rs) = helper ch' rs
          where
            ch' = M.insert r' 'O' . M.insert r '.' $ ch

            r' = case takeWhile ((=='.') . (ch M.!)) [(rx,ry-dy) | dy <- [1..ry]] of
              [] -> r
              l  -> last l