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

solve (seeds, converters) = unlines [
  show seeds
  , unlines . map show $ converters
  , "converted"
  , unlines . map show $ converted
  , show answer
  ]
  where
    converted = foldl convertAll seeds converters
      where
        convertAll nums conv = map (convert conv) nums
        
        convert ((destStart, sourceStart, length):cs) v
          | sourceStart <= v && v-sourceStart < length = destStart + v - sourceStart
          | otherwise = convert cs v
        convert [] v = v

    answer = minimum converted
