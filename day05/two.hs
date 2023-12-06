import Data.Char
import Text.Parsec
import Parsing

main :: IO ()
main = optimisticInteract readD solve

readD :: Parser ([Integer], [[(Integer, Integer, Integer)]])
readD = readAll
  where
    readAll = do
      seeds <- readSeeds
      many1 newline
      converters <- readConverters `sepBy` newline
      return (seeds, converters)

    readSeeds = do
      string "seeds: "
      seeds <- number `sepBy` char ' '
      return seeds

    readConverters = do
      many1 (oneOf $ ['a'..'z']++['A'..'Z']++":- ")
      newline
      converters <- readConverter `endBy` newline
      return converters

    readConverter = do
      [a, b, c] <- number `sepBy` char ' '
      return (a,b,c)

solve :: ([Integer], [[(Integer, Integer, Integer)]]) -> String
solve (seeds, converters) = unlines [
  show seeds
  , show (length actualSeeds)
  , unlines . map show $ converters
  , "converted"
  , unlines . map show $ converted
  , show answer
  ]
  where
    actualSeeds = genSeeds seeds
      where
        genSeeds (s:l:ss) = (s,l) : genSeeds ss
        genSeeds [] = []

    converted = scanl convert actualSeeds converters
      where
        convert v [] = v
        convert ranges ((destStart, sourceStart, clength):cs) = intersectedRanges ++ convert otherRanges cs
          where
            intersectedRanges = map transform . concatMap (intersect (sourceStart, clength)) $ ranges
            transform (start, length) = (start-sourceStart+destStart, length)
            otherRanges = concatMap (`remove` (sourceStart, clength)) $ ranges

            intersect (s1, l1) (s2, l2)
              | minEnd - maxStart > 0 = [(maxStart, minEnd-maxStart)]
              | otherwise   = []
              where
                maxStart = max s1 s2
                minEnd = min (s1+l1) (s2+l2)

            remove (s1, l1) (s2, l2)
              | l1 <= 0    = []
              | s1 < s2    = (s1, min l1 (s2-s1)) : remove (s2, l1 - (s2-s1)) (s2, l2)
              | s1 < s2+l2 = remove (s2+l2, s1 + l1 - s2 - l2) (s2, l2)
              | otherwise  = [(s1, l1)]

    answer = minimum $ last converted
