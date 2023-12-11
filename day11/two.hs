import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[Char]]
readD = (many1 $ oneOf ".#") `sepEndBy` newline


solve galaxy = unlines [
  show $ galaxy
  , show $ answer
  ]
  where
    galaxyM = readM galaxy

    (expandedX, expandedY) = (findEmpty $ transpose galaxy, findEmpty galaxy)

    findEmpty g = map fst . filter helper $ zip [0..] g
      where
        helper (_,r) = not . any (=='#') $ r

    expandGalaxy factor = helper galaxyM (reverse $ sort expandedX) (reverse $ sort expandedY)
      where
        growth = factor-1

        helper gm (ex:exs) eys = helper (expandX gm ex) exs eys
        helper gm exs (ey:eys) = helper (expandY gm ey) exs eys
        helper gm [] [] = gm

        expandX gm ex = M.mapKeys (\(x,y) -> (expandD ex x, y)) gm
        expandY gm ey = M.mapKeys (\(x,y) -> (x, expandD ey y)) gm

        expandD eD d
          | d > eD    = d + growth
          | otherwise = d

    manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs(y1-y2)

    galaxyPos = M.keys . M.filter (=='#') $ expandGalaxy 1000000

    answer = sum [manhattan a b | (i, a) <- zip [1..] galaxyPos, b <- drop i galaxyPos]
