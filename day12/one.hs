import Text.Parsec
import Parsing
import Generic

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

    arrs (springs, groups) = helper springs groups
      where
        helper ('.':ss) gs = helper ss gs
        helper [] (g:_)  = 0
        helper sprs []
          | all (`elem` ".?") sprs = 1
          | otherwise              = 0
        helper sprs@(s:ss) grps@(g:gs)
          | cangroup sprs g = ifSkip + helper (dropN g ss) gs
          | otherwise       = ifSkip
          where
            ifSkip = if s == '#' then 0 else helper ss grps

    cangroup s g = lengthN possible >= g && next /= '#'
      where
        possible = takeWhile (`elem` "?#") s
        next = head . dropN g $ s ++ "."
