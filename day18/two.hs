import Text.Parsec
import Parsing
import Chart2d
import qualified Data.HashMap.Strict as M
import Data.Char

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser [(Char, Integer, String)]
readD = readLine `sepEndBy` newline
  where
    readLine = do
      c <- oneOf "RLUD"
      spaces
      n <- number
      string " (#"
      s <- many1 alphaNum
      char ')'
      return (c, n, s)

solve input = unlines [
  show $ input
  , show $ realInput
  , show $ inpath
  , show $ answer
  ]
  where
    answer = area inpath

    realInput = [(getD hd, readHex hn) | (_, _, h) <- input, let (hn, hd) = (splitAt 5) h]
      where
        getD "0" = 'R'
        getD "1" = 'D'
        getD "2" = 'L'
        getD "3" = 'U'

        readHex hex = foldl (\acc c -> 16*acc + read1H c) 0 hex
        
        read1H c
          | c `elem` "0123456789" = toInteger $ ord c - ord '0'
          | c `elem` "abcdef"     = toInteger $ ord c - ord 'a' + 10

    area path = innerArea `div` 2 + tiles `div` 2 + 1
      where
        innerArea = sum $ zipWith calcArea path (tail path)
        calcArea (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

        tiles = sum $ zipWith manhattan path (tail path)
        manhattan (x1,y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

    inpath = mapout realInput

    mapout inps = scanl move (0,0) inps
      where
        move (x,y) ('L', n) = (x-n,y)
        move (x,y) ('R', n) = (x+n,y)
        move (x,y) ('U', n) = (x,y-n)
        move (x,y) ('D', n) = (x,y+n)
