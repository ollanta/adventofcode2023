import Data.Char
import Text.Parsec
import Parsing
import qualified Data.HashMap.Strict as M
import Chart2d
import Data.List

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [([Char],[Integer])]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      str <- many1 (oneOf ".#?")
      space
      nums <- number `sepBy` char ','
      return (str, nums)

solve input = unlines [
  show $ input
  , show . map arrs $ input
  , show $ answer
  ]
  where
    answer = sum . map arrs $ input

    arrs (springs, groups) = helper springs (map fromInteger groups)
      where
        helper ('.':ss) gs = helper ss gs
        helper sprs@('#':ss) (g:gs)
          | subarr sprs g = helper (drop g ss) gs
          | otherwise     = 0
        helper ('#':ss) [] = 0
        helper sprs@('?':ss) (g:gs)
          | subarr sprs g = ifSkip + helper (drop g ss) gs
          | otherwise     = ifSkip
          where
            ifSkip = helper ss (g:gs)
        helper ('?':ss) [] = helper ss []
        helper [] (g:gs) = 0
        helper [] [] = 1

    subarr s g = length possible >= g && (length s == g || next /= '#')
      where
        possible = takeWhile (`elem` "?#") s
        next = head . drop g $ s
        {-}
    subarr ('.':cs) g = g <= 0
    subarr []       g = g <= 0
    subarr ('?':c:cs) 0 = c /= '#'
    subarr ('#':cs) g = subarr cs (g-1)
    subarr ('?':cs) g = subarr cs (g-1)
    -}
