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
  , show $ arrlist
  , show $ answer
  ]
  where
    arrlist = map (fst . arrs) $ map (\(springs, groups) -> (tail . concat $ replicate 5 ('?':springs), concat $ replicate 5 groups)) input
    answer = sum arrlist

    arrs (springs, groups) = helper M.empty springs (map fromInteger groups)
      where
        helper :: M.HashMap ([Char],[Int]) Integer -> [Char] -> [Int] -> (Integer, M.HashMap ([Char],[Int]) Integer)
        helper mem sprs gs
          | (sprs,gs) `M.member` mem = (mem M.! (sprs,gs), mem)
          | sum gs > length (filter (/='.') sprs) = (0, mem)
        helper mem ('.':ss) gs = helper mem ss gs
        helper mem sprs@('#':ss) (g:gs)
          | subarr sprs g = (val, mem'')
          | otherwise     = (0, M.insert (sprs, g:gs) 0 mem)
          where
            (val, mem') = helper mem (drop g ss) gs
            mem'' = M.insert (sprs, g:gs) val mem'
        helper mem ('#':ss) [] = (0, mem)
        helper mem sprs@('?':ss) (g:gs)
          | subarr sprs g = (ifSkip + notSkip, mem''')
          | otherwise     = (ifSkip, M.insert (sprs, g:gs) ifSkip mem')
          where
            (ifSkip, mem') = helper mem ss (g:gs)
            (notSkip, mem'') = helper mem' (drop g ss) gs
            mem''' = M.insert (sprs, g:gs) (ifSkip + notSkip) mem''
        helper mem ('?':ss) [] = helper mem ss []
        helper mem [] (g:gs) = (0, mem)
        helper mem [] [] = (1, mem)

    subarr s g = length possible >= g && (length s == g || next /= '#')
      where
        possible = takeWhile (`elem` "?#") s
        next = head . drop g $ s