import Text.Parsec
import Parsing
import Generic
import qualified Data.HashMap.Lazy as L
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
    longerInput = [(intercalate "?" $ replicate 5 springs, concat $ replicate 5 groups) | (springs, groups) <- input]
    arrlist = map arrs longerInput
    answer = sum arrlist

    arrs (springs, groups) = mhelper springs groups
      where
        mhelper sprs grps = memo L.! (sprs, grps)
          where
            memo = L.fromList [((sprs, grps), helper sprs grps) | sprs <- tails springs, grps <- tails groups]

        helper ('.':ss) gs = mhelper ss gs
        helper [] (g:_) = 0
        helper sprs []
          | all (`elem` ".?") sprs = 1
          | otherwise              = 0
        helper sprs@(s:ss) grps@(g:gs)
          | cangroup sprs g = ifSkip + mhelper (dropN g ss) gs
          | otherwise       = ifSkip
          where
            ifSkip = if s == '#' then 0 else mhelper ss grps

    cangroup s g = lengthN possible >= g && next /= '#'
      where
        possible = takeWhile (`elem` "?#") s
        next = head . dropN g $ s ++ "."

