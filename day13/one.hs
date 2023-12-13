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
  , show $ answer
  ]
  where
    answer = sum . map (score . allReflections) $ input

    score ([v], []) = v
    score ([], [h]) = 100*h

    allReflections rows = (reflections rows, reflections $ transpose rows)

    reflections rows = union . map helper' $ rows
      where
        union :: (Eq a) => [[a]] -> [a]
        union (a:b:cs) = union ((filter (`elem` a) b):cs)
        union (a:_) = a

        helper' row = [i | i <- [1..length row - 1], reflects (take i row) (drop i row)]

        reflects l1 l2 = and $ zipWith (==) (reverse l1) l2