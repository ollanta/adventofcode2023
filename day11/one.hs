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
    expand = transpose . concatMap helper . transpose . concatMap helper
      where
        helper rc
          | any (=='#') rc = [rc]
          | otherwise      = [rc, rc]

    manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs(y1-y2)

    galaxyPos = M.keys . M.filter (=='#') . readM $ expand galaxy

    answer = sum [manhattan a b | (i, a) <- zip [1..] galaxyPos, b <- drop i galaxyPos]
