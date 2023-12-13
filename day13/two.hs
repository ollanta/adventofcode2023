import Text.Parsec
import Parsing
import Generic
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [[String]]
readD = readChart `sepEndBy` newline
  where
    readChart = (many1 $ oneOf ".#") `sepEndBy` newline

solve input = unlines [
  show $ input
  , show $ map allReflections input
  , show $ answer
  ]
  where
    answer = sum . map (score . allReflections) $ input

    score ([v], []) = v
    score ([], [h]) = 100*h

    allReflections rows = (reflections rows, reflections $ transpose rows)

    reflections rows = helper' rows
      where
        l = length . head $ rows

        helper' rows = [i | i <- [1..l - 1], reflects (map (take i) rows) (map (drop i) rows)]

        reflects ls1 ls2 = smudges == 1
          where
            smudges = length . filter (==False) $ zipWith (==) ls1' ls2'
            shortest = minimum . map (length . head) $ [ls1, ls2]
            ls1' = concat $ map (take shortest . reverse) ls1
            ls2' = concat $ map (take shortest) ls2